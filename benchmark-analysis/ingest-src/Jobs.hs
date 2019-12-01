{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jobs
    ( Validation(..)
    , (.>)
    , variantToPropertyJob
    , processProperty
    , missingRunToTimingJob
    , processTiming
    , validationMissingRuns
    , validateResults
    , cleanupValidation
    ) where

import BroadcastChan.Conduit
import qualified Control.Concurrent.STM as STM
import Control.Monad (unless, when)
import Control.Monad.Trans.Resource (register, release)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit (ConduitT, (.|), runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (removeFile)

import Core
import MissingQuery (MissingRun(..), ValidationVariant(..), validationRunQuery)
import Parsers
import ProcessPool (Job, Result(..), makeJob)
import Query (runSqlQuery)
import qualified RuntimeData
import Schema
import Sql (Entity(..), Key, MonadSql, Transaction, (=.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans
import Types (HashDigest)

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: HashDigest) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

variantToPropertyJob
    :: Entity Variant
    -> ConduitT (Entity Variant)
                (Job (Key Algorithm, Key Graph, Maybe Hash))
                SqlM
                ()
variantToPropertyJob
    (Entity varId (Variant graphId variantCfgId _ hash hasProps retries)) =
        case hash of
            Nothing
                | retries < 5 -> yieldJob
                | otherwise -> logWarnN $
                    "Hash missing, but too many retries for variant #"
                    <> showSqlKey varId

            Just _
                | not hasProps && retries < 5 -> yieldJob
                | not hasProps -> logWarnN $
                    "Too many retries for variant #" <> showSqlKey varId
                | otherwise -> return ()
  where
    yieldJob = do
        Graph _ path _ _ <- Sql.getJust graphId
        VariantConfig algoId _ flags _ <- Sql.getJust variantCfgId
        Algorithm algo _ <- Sql.getJust algoId
        yield . makeJob (algoId,graphId,hash) varId Nothing $
            [ "-a", algo
            , "-k switch --log"
            , showSqlKey varId <> ".log"
            , fromMaybe "" flags
            , path
            ]

processProperty :: Result (Key Algorithm, Key Graph, Maybe Hash) -> SqlM ()
processProperty Result{resultValue=(algoId, graphId, hash), ..} = do
    logDebugNS "Property#Start" resultLabel
    liftIO $ removeFile timingFile

    SqlTrans.runTransaction $ do
        resultHash <- computeHash outputFile
        loadProps <- case hash of
            Nothing -> True <$ SqlTrans.update resultVariant
                                    [VariantResult =. Just resultHash]

            Just prevHash | prevHash == resultHash -> return True
            _ -> False <$ logErrorN
                    ("Hash mismatch for variant: " <> showSqlKey resultVariant)

        liftIO $ removeFile outputFile

        when loadProps $ do
            runConduit $
                C.sourceFile propLog
                .| C.decode C.utf8
                .| C.map (T.replace "," "")
                .| conduitParse property
                .| C.mapM_ insertProperty

            SqlTrans.update resultVariant [VariantPropsStored =. True]

        liftIO $ removeFile propLog

    logDebugNS "Property#End" resultLabel
  where
    propLog :: FilePath
    propLog = T.unpack resultLabel <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack resultLabel <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack resultLabel <> ".output"

    insertProperty :: Property -> Transaction SqlM ()
    insertProperty (GraphProperty name val) =
        SqlTrans.insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) = do
        SqlTrans.insertUniq $ StepProp algoId name
        SqlTrans.insertUniq $ StepPropValue algoId resultVariant n name val

    insertProperty Prediction{} = return ()

missingRunToTimingJob
    :: (MonadLogger m, MonadResource m, MonadSql m)
    => Key Platform
    -> MissingRun (Maybe Hash)
    -> ConduitT (MissingRun (Maybe Hash))
                (Job (Key Algorithm, Key Implementation, Hash))
                m
                ()
missingRunToTimingJob platformId MissingRun{..}
  | Nothing <- missingRunExtraInfo
  = logErrorN . mconcat $
        [ "Algorithm #", showSqlKey missingRunAlgorithmId
        , " results missing for variant #", showSqlKey missingRunVariantId
        ]

  | Just hash <- missingRunExtraInfo = yield $ makeJob
            (missingRunAlgorithmId, missingRunImplId, hash)
            missingRunVariantId
            (Just (platformId, missingRunImplName))
            missingRunArgs

processTiming
    :: (MonadLogger m, MonadResource m, MonadSql m, MonadThrow m)
    => Key RunConfig
    -> Text
    -> Result (Key Algorithm, Key Implementation, Hash)
    -> m ()
processTiming runConfigId commit Result{..} = do
    logDebugNS "Timing#Start " resultLabel
    time <- liftIO getCurrentTime
    resultHash <- computeHash outputFile
    liftIO $ removeFile outputFile

    if commit /= resultAlgorithmVersion
       then logErrorN $ mconcat
        [ "Unexpected algorithm version for implementation #"
        , showSqlKey implId, "! Expected commit ", commit, " found commit "
        , resultAlgorithmVersion
        ]
       else SqlTrans.runTransaction $ do
        let validated = resultHash == hash

        runId <- SqlTrans.insert $
            Run runConfigId resultVariant implId algoId time validated

        unless validated $ do
            logErrorN . mconcat $
                [ "Implementation #", showSqlKey implId
                , " has wrong result hash for variant #"
                , showSqlKey resultVariant
                , " for run config #", showSqlKey runConfigId
                ]

        runConduit $
            C.sourceFile timingFile
            .| C.decode C.utf8
            .| C.map (T.replace "," "")
            .| conduitParse timer
            .| C.mapM_ (insertTiming runId)

        logDebugNS "Timing#End" resultLabel

    liftIO $ removeFile timingFile
  where
    (algoId, implId, hash) = resultValue
    timingFile = T.unpack resultLabel <> ".timings"
    outputFile = T.unpack resultLabel <> ".output"

    insertTiming :: MonadSql m => Key Run -> Timer -> Transaction m ()
    insertTiming runId (TotalTiming Timing{..}) = SqlTrans.insert_ $
        TotalTimer runId name minTime avgTime maxTime stddev

    insertTiming runId (StepTiming n Timing{..}) = SqlTrans.insert_ $
        StepTimer runId n name minTime avgTime maxTime stddev

data Validation = Validation
    { cleanData :: SqlM ()
    , originalCommit :: Text
    , referenceResult :: FilePath
    , runId :: Key Run
    }

validationMissingRuns
    :: Key Platform
    -> Result ValidationVariant
    -> ConduitT (Result ValidationVariant) (Job Validation) SqlM ()
validationMissingRuns platformId Result{..} = do
    liftIO $ removeFile timingFile
    refCounter <- liftIO $ STM.newTVarIO validationMissingCount
    key <- register $ removeFile outputFile

    let onCompletion :: SqlM ()
        onCompletion = do
            count <- liftIO . STM.atomically $ do
                STM.modifyTVar' refCounter (subtract 1)
                STM.readTVar refCounter

            when (count == 0) $ release key

        mkValidation :: Key Run -> Validation
        mkValidation = Validation onCompletion validationCommit outputFile

        toValidationJob :: MissingRun (Key Run) -> Job Validation
        toValidationJob MissingRun{..} = makeJob
            (mkValidation missingRunExtraInfo)
            missingRunVariantId
            (Just (platformId, missingRunImplName))
            missingRunArgs

    runSqlQuery (validationRunQuery platformId resultValue)
        .| C.map toValidationJob
  where
    ValidationVariant{..} = resultValue
    outputFile = T.unpack resultLabel <> ".output"
    timingFile = T.unpack resultLabel <> ".timings"

validateResults
    :: Int -> ConduitT (Result Validation) (Result Validation) SqlM ()
validateResults numProcs = do
    validate <- RuntimeData.getOutputChecker
    parMapM (Simple Terminate) numProcs (process validate)
  where
    process
        :: (FilePath -> FilePath -> SqlM Bool)
        -> Result Validation
        -> SqlM (Result Validation)
    process check res@Result{resultAlgorithmVersion, resultLabel, resultValue}
      | originalCommit /= resultAlgorithmVersion = do
            res <$ logErrorN "Result validation used wrong algorithm version!"
      | otherwise = do
            result <- check referenceResult outputFile
            ts <- liftIO $ getCurrentTime

            when result $ do
                Sql.update runId [RunValidated =. True, RunTimestamp =. ts]
            return res
      where
        Validation{..} = resultValue
        outputFile = T.unpack resultLabel <> ".output"

cleanupValidation :: Result Validation -> SqlM ()
cleanupValidation Result{resultLabel, resultValue} = do
    liftIO $ do
        removeFile outputFile
        removeFile timingFile
    cleanData
  where
    Validation{..} = resultValue

    timingFile = T.unpack resultLabel <> ".timings"
    outputFile = T.unpack resultLabel <> ".output"
