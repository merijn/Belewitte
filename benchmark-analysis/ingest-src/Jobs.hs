{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jobs
    ( (.>)
    , variantToPropertyJob
    , processProperty
    , missingRunToTimingJob
    , processTiming
    ) where

import Control.Monad (unless, when)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit (ConduitT, (.|), awaitForever, runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (removeFile)

import Core
import MissingQuery (MissingRun(..))
import Parsers
import ProcessPool (Job(..), Result(..))
import Schema
import Sql (Entity(..), Key, MonadSql, (=.))
import qualified Sql

(.>) :: Monad m
      => ConduitT a b m ()
      -> (b -> ConduitT b c m r)
      -> ConduitT a c m ()
producer .> consumer = producer .| awaitForever consumer
infixl 3 .>

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: Digest MD5) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

variantToPropertyJob
    :: Entity Variant
    -> ConduitT (Entity Variant)
                (Job (Key Graph, Maybe Hash))
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
        yield . Job (graphId,hash) varId (showSqlKey varId) . T.unwords $
            [ showSqlKey varId
            , "-a", algo
            , "-k switch --log"
            , showSqlKey varId <> ".log"
            , fromMaybe "" flags
            , path
            ]

processProperty :: Result (Key Graph, Maybe Hash) -> SqlM ()
processProperty Result{resultValue=(graphId, hash), ..} = do
    logInfoN $ "Property: " <> resultLabel
    liftIO $ removeFile timingFile

    resultHash <- computeHash outputFile
    loadProps <- case hash of
        Nothing ->
            True <$ Sql.update resultVariant [VariantResult =. Just resultHash]

        Just prevHash | prevHash == resultHash -> return True
        _ -> False <$ logErrorN
                ("Hash mismatch for variant: " <> showSqlKey resultVariant)

    when loadProps $ do
        liftIO $ removeFile outputFile

        runConduit $
            C.sourceFile propLog
            .| C.decode C.utf8
            .| C.map (T.replace "," "")
            .| conduitParse property
            .| C.mapM_ insertProperty

        liftIO $ removeFile propLog
        Sql.update resultVariant [VariantPropsStored =. True]

    logInfoN $ "Property done: " <> resultLabel
  where
    propLog :: FilePath
    propLog = T.unpack resultLabel <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack resultLabel <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack resultLabel <> ".output"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        Sql.insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        Sql.insertUniq $ StepProp resultVariant n name val

    insertProperty Prediction{} = return ()

missingRunToTimingJob
    :: (MonadLogger m, MonadResource m, MonadSql m)
    => MissingRun
    -> ConduitT MissingRun (Job (Key Algorithm, Key Implementation, Hash)) m ()
missingRunToTimingJob MissingRun{..}
  | Nothing <- missingRunVariantResult
  = logErrorN . mconcat $
        [ "Algorithm #", showSqlKey missingRunAlgorithmId
        , " results missing for variant #", showSqlKey missingRunVariantId
        ]

  | Just hash <- missingRunVariantResult = yield $ job hash
  where
    tag name = mconcat [ showSqlKey missingRunVariantId, " ", name]
    implName = tag missingRunImplName
    cmd = T.unwords $ "\"" <> implName <> "\"" : missingRunArgs
    job hash = Job (missingRunAlgorithmId, missingRunImplId, hash) missingRunVariantId implName cmd

processTiming
    :: (MonadLogger m, MonadResource m, MonadSql m, MonadThrow m)
    => Key RunConfig
    -> Text
    -> Result (Key Algorithm, Key Implementation, Hash)
    -> m ()
processTiming runConfigId commit Result{..} = do
    logInfoN $ "Timing: " <> resultLabel
    time <- liftIO getCurrentTime
    resultHash <- computeHash outputFile
    liftIO $ removeFile outputFile

    if commit /= resultAlgorithmVersion
       then logErrorN $ mconcat
        [ "Unexpected algorithm version for implementation #"
        , showSqlKey implId, "!. Expected commit ", commit, " found commit "
        , resultAlgorithmVersion
        ]
       else do
        let validated = resultHash == hash

        runId <- Sql.insert $ Run runConfigId resultVariant implId algoId time validated

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

        logInfoN $ "Timing done: " <> resultLabel

    liftIO $ removeFile timingFile
  where
    (algoId, implId, hash) = resultValue
    timingFile = T.unpack resultLabel <> ".timings"
    outputFile = T.unpack resultLabel <> ".output"

    insertTiming :: MonadSql m => Key Run -> Timer -> m ()
    insertTiming runId (TotalTiming Timing{..}) = Sql.insert_ $
        TotalTimer runId name minTime avgTime maxTime stddev

    insertTiming runId (StepTiming n Timing{..})= Sql.insert_ $
        StepTimer runId n name minTime avgTime maxTime stddev
