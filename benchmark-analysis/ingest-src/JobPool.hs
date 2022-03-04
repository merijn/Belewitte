{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module JobPool
    ( Job(jobValue)
    , Result(..)
    , makePropertyJob
    , makeTimingJob
    , cleanupTimings
    , cleanupOutput
    , cleanupProperties
    , processJobsParallel
    , processJobsParallelWithSharedPool
    , withProcessPool
    ) where

import Control.Monad.Catch (onError, try)
import Control.Monad.Trans.Resource (ReleaseKey, register, release)
import Data.Conduit (ConduitT)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory (removeFile)
import System.FilePath ((<.>))
import System.IO.Error (isDoesNotExistError)

import BroadcastChan.Conduit
import Core
import ProcessTools.Pool hiding (withProcessPool)
import qualified ProcessTools.Pool as Pool
import RuntimeData (getKernelExecutable, getKernelLibPath)
import Query (MonadQuery)
import Query.Missing (maxRetryCount)
import Schema
import Sql (MonadSql, (+=.), getGlobalVar)
import qualified Sql

data Job a = Job
    { jobValue :: a
    , jobVariant :: Key Variant
    , jobLabel :: Text
    , jobCommand :: Text
    , jobLogProperties :: Bool
    } deriving (Functor, Foldable, Show, Traversable)

makeJob
    :: Bool
    -> a
    -> Key Variant
    -> Maybe (Key Platform, Text)
    -> [Text]
    -> Job a
makeJob logProps val variantId implName args = Job
    { jobValue = val
    , jobVariant = variantId
    , jobLabel = label
    , jobCommand = T.unwords $ "\"" <> label <> "\"" : finalArgs
    , jobLogProperties = logProps
    }
  where
    finalArgs
        | logProps = "-k switch --log" : logFile : args
        | otherwise = args

    logFile = "\"" <> label <> ".log" <> "\""

    label = case implName of
        Nothing -> showSqlKey variantId
        Just (platformId, name) -> mconcat
            [ showSqlKey platformId, " ", showSqlKey variantId, " ", name ]

makePropertyJob
    :: a -> Key Variant -> Maybe (Key Platform, Text) -> [Text] -> Job a
makePropertyJob = makeJob True

makeTimingJob
    :: a -> Key Variant -> Maybe (Key Platform, Text) -> [Text] -> Job a
makeTimingJob = makeJob False

data Result a = Result
    { resultValue :: a
    , resultVariant :: Key Variant
    , resultLabel :: Text
    , resultAlgorithmVersion :: CommitId
    , resultOutput :: (FilePath, ReleaseKey)
    , resultTimings :: (FilePath, ReleaseKey)
    , resultPropLog :: Maybe (FilePath, ReleaseKey)
    } deriving (Functor, Foldable, Traversable)

cleanupOutput :: MonadIO m => Result a -> m ()
cleanupOutput Result{resultOutput = (_, key)} = release key

cleanupTimings :: MonadIO m => Result a -> m ()
cleanupTimings Result{resultTimings = (_, key)} = release key

cleanupProperties :: MonadIO m => Result a -> m ()
cleanupProperties Result{resultPropLog} = case resultPropLog of
    Just (_, key) -> release key
    Nothing -> return ()

jobTask
    :: (MonadLogger m, MonadMask m, MonadSql m)
    => Task m (Job a) (Result a)
jobTask = Task
    { taskGenJob = jobCommand
    , taskHandleErrors = \Job{..} act -> act `onError` do
            let fileStem = T.unpack jobLabel
                outputFile = fileStem <.> "output"
                timingFile = fileStem <.> "timings"
                logFile = fileStem <.> "log"

            logErrorN ("Failed: " <> jobCommand)
            tryRemoveFile $ outputFile
            tryRemoveFile $ timingFile
            tryRemoveFile $ logFile

    , taskGenResult = \Job{..} result -> do
        let fileStem = T.unpack jobLabel
            outputFile = fileStem <.> "output"
            timingFile = fileStem <.> "timings"
            logFile = fileStem <.> "log"

            (label, commit) = case T.splitOn ":" result of
                (version:rest) -> (T.concat rest, version)
                [] -> ("", "")

        outputKey <- registerFile outputFile
        timingKey <- registerFile timingFile
        logKey <- if jobLogProperties
                        then Just <$> registerFile logFile
                        else return Nothing

        return $ Result
            { resultValue = jobValue
            , resultVariant = jobVariant
            , resultLabel = label
            , resultAlgorithmVersion = CommitId commit
            , resultOutput = outputKey
            , resultTimings = timingKey
            , resultPropLog = logKey
            }

    , taskHandler = Handle handler
    }
  where
    checkNotExist :: SomeException -> Bool
    checkNotExist e = fromMaybe False $ isDoesNotExistError <$> fromException e

    registerFile :: MonadResource m => FilePath -> m (FilePath, ReleaseKey)
    registerFile path = fmap (path,) . register $ removeFile path

    tryRemoveFile path = do
        result <- try . liftIO $ removeFile path
        case result of
            Left exc
                | checkNotExist exc -> return ()
                | otherwise -> logErrorN . T.pack $ displayException exc
            Right () -> return ()

    handler :: MonadSql m => Job a -> SomeException -> m Action
    handler Job{jobVariant} exc
      | Just Timeout <- fromException exc = return Retry
      | otherwise = do
          retries <- variantRetryCount <$> Sql.getJust jobVariant
          if retries >= maxRetryCount
             then return Drop
             else Retry <$ Sql.update jobVariant [ VariantRetryCount +=. 1 ]

runnerCreator :: MonadQuery m => Platform -> m Runner
runnerCreator Platform{platformName,platformFlags}= do
    exePath <- getKernelExecutable
    libPath <- getKernelLibPath

    let args = [exePath, "-L", libPath, "-W", "-S"]

    result <- getGlobalVar RunCommand
    case result of
        Just cmd -> return $ customRunner cmd flags args
        Nothing -> return $ defaultGpuRunner flags args
  where
    flags = map T.unpack $ case platformFlags of
        Nothing -> ["-C", platformName]
        Just txt -> T.splitOn " " txt

withProcessPool
    :: (MonadLoggerIO m, MonadQuery m)
    => Int -> Platform -> (Pool Process -> m a) -> m a
withProcessPool n = Pool.withProcessPool n . runnerCreator

processJobsParallel
    :: (MonadLoggerIO m, MonadMask m, MonadQuery m, MonadUnliftIO m)
    => Int -> Platform -> ConduitT (Job a) (Result a) m ()
processJobsParallel numNodes =
    processTasksParallel jobTask numNodes . runnerCreator

processJobsParallelWithSharedPool
    :: (MonadLoggerIO m, MonadMask m, MonadSql m, MonadUnliftIO m)
    => Int -> Pool Process -> ConduitT (Job a) (Result a) m ()
processJobsParallelWithSharedPool = processTasksParallelWithSharedPool jobTask
