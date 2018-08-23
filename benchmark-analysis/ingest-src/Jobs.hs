{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jobs (propertyJobs, processProperty, timingJobs, processTiming) where

import Control.Monad (when)
import Control.Monad.Logger (logErrorN, logInfoN)
import Control.Monad.Trans (lift)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit
    (ConduitT, (.|), awaitForever, runConduit, toProducer, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Sqlite (Key, Entity(..), (=.), (==.))
import qualified Database.Persist.Sqlite as Sql
import System.Directory (removeFile)

import Core
import Parsers
import Schema

(.|>) :: Monad m
      => ConduitT () b m ()
      -> (b -> ConduitT b c m r)
      -> ConduitT a c m ()
producer .|> consumer = toProducer producer .| awaitForever consumer

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: Digest MD5) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

propertyJobs :: ConduitT () (Key Variant, Key Graph, Maybe Hash, Text) SqlM ()
propertyJobs = Sql.selectSource [] [] .|> toPropJob
  where
    toPropJob
        :: Entity Variant
        -> ConduitT (Entity Variant)
                    (Key Variant, Key Graph, Maybe Hash, Text)
                    SqlM
                    ()
    toPropJob (Entity varId (Variant graphId algoId _ varFlags hash)) =
        case hash of
            Nothing -> yieldJob
            Just _ -> whenNotExists filters yieldJob
      where
        filters = [StepPropVariantId ==. varId, StepPropStepId ==. 0]
        yieldJob = do
            Graph _ path _ <- lift $ Sql.getJust graphId
            Algorithm algo _ <- lift $ Sql.getJust algoId
            yield . (varId, graphId, hash,) . T.intercalate " " $
                [ showSqlKey varId
                , "-a", algo
                , "-k switch --log"
                , showSqlKey varId <> ".log"
                , fromMaybe "" varFlags
                , path
                ]

processProperty :: (Key Variant, Key Graph, Maybe Hash, Text) -> SqlM ()
processProperty (varId, graphId, hash, var) = do
    logInfoN $ "Property: " <> var
    liftIO $ removeFile timingFile

    resultHash <- computeHash outputFile
    loadProps <- case hash of
        Nothing -> True <$ Sql.update varId [VariantResult =. Just resultHash]
        Just prevHash | prevHash == resultHash -> return True
        _ -> False <$ logErrorN
                ("Hash mismatch for variant: " <> showSqlKey varId)

    when loadProps $ do
        liftIO $ removeFile outputFile

        runConduit $
            C.sourceFile propLog
            .| C.decode C.utf8
            .| C.map (T.replace "," "")
            .| conduitParse property
            .| C.mapM_ insertProperty

        liftIO $ removeFile propLog

    logInfoN $ "Property done: " <> var
  where
    propLog :: FilePath
    propLog = T.unpack var <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack var <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack var <> ".output"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        insertUniq $ StepProp varId n name val

    insertProperty Prediction{} = return ()

timingJobs
    :: Int -> Key GPU
    -> ConduitT () (Key Variant, Key Implementation, Hash, Text) SqlM ()
timingJobs numRuns gpuId = do
    Sql.selectKeys [] [] .|> \algoKey ->
        Sql.selectSource [VariantAlgorithmId ==. algoKey] [] .|> toTimingJob
  where
    toTimingJob
        :: Entity Variant
        -> ConduitT (Entity Variant)
                    (Key Variant, Key Implementation, Hash, Text)
                    SqlM
                    ()
    toTimingJob (Entity varId (Variant graphId algoId _ _ Nothing)) = do
        logErrorN . mconcat $
            [ "Algorithm #", showSqlKey algoId, " results missing for graph #"
            , showSqlKey graphId, " variant #", showSqlKey varId ]

    toTimingJob (Entity varId (Variant graphId algoId _ varFlags (Just hash)))
      = Sql.selectSource [ImplementationAlgorithmId ==. algoId] [] .|> runImpl
      where
        runImpl (Entity implId (Implementation _ name _ implFlags _ _)) = do
            whenNotExists (filters ++ [TotalTimerImplId ==. implId]) $ do
                Graph _ path _ <- lift $ Sql.getJust graphId
                Algorithm algo _ <- lift $ Sql.getJust algoId

                yield . (varId, implId, hash,) . T.intercalate " " $
                    [ tag name , "-a"
                    , algo
                    , fromMaybe ("-k " <> name) implFlags
                    , fromMaybe "" varFlags
                    , "-n " <> showText numRuns
                    , path
                    ]

        filters = [TotalTimerGpuId ==. gpuId, TotalTimerVariantId ==. varId]
        tag name = mconcat ["\"", showSqlKey varId, " ", name, "\""]

processTiming
    :: Key GPU -> (Key Variant, Key Implementation, Hash, Text) -> SqlM ()
processTiming gpuId (varId, implId, hash, var) = do
    logInfoN $ "Timing: " <> var
    timestamp <- liftIO getCurrentTime
    resultHash <- computeHash outputFile

    if resultHash /= hash
       then do
           logErrorN . mconcat $
            [ "Implementation #", showSqlKey implId
            , " has wrong results for variant #", showSqlKey varId, " on GPU #"
            , showSqlKey gpuId ]
        else do
            liftIO $ removeFile outputFile
            runConduit $
                C.sourceFile timingFile
                .| C.decode C.utf8
                .| C.map (T.replace "," "")
                .| conduitParse timer
                .| C.mapM_ (insertTiming timestamp)
            liftIO $ removeFile timingFile

    logInfoN $ "Timing done: " <> var
  where
    timingFile = T.unpack var <> ".timings"
    outputFile = T.unpack var <> ".output"

    insertTiming :: UTCTime -> Timer -> SqlM ()
    insertTiming ts (TotalTiming Timing{..}) = Sql.insert_ $
        TotalTimer gpuId varId implId name minTime avgTime maxTime stddev ts

    insertTiming ts (StepTiming n Timing{..}) = Sql.insert_ $
        StepTimer gpuId varId n implId name minTime avgTime maxTime stddev ts
