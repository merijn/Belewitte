{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module ProcessPool
    ( Job(jobValue)
    , Result(..)
    , Process(Process,inHandle,outHandle)
    , makePropertyJob
    , makeTimingJob
    , cleanupTimings
    , cleanupOutput
    , cleanupProperties
    , processJobsParallel
    , processJobsParallelWithSharedPool
    , withProcessPool
    ) where

import Control.Monad (guard, unless, void)
import Control.Monad.Catch (onError, try, uninterruptibleMask_)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans.Resource (ReleaseKey, allocate, register, release)
import Data.Acquire (withAcquire, mkAcquireType, ReleaseType(ReleaseException))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import Data.Conduit (ConduitT)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.LocalTime as Time
import Data.Time.Calendar (DayOfWeek(Saturday,Sunday), dayOfWeek)
import Network.HostName (getHostName)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>))
import System.IO (BufferMode(LineBuffering), Handle)
import qualified System.IO as System
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess(..), ProcessHandle, Pid, StdStream(..))
import qualified System.Process as Proc

import BroadcastChan.Conduit
import Core
import ProcessTools (UnexpectedTermination, unexpectedTermination)
import RuntimeData (getKernelExecutable, getKernelLibPath)
import Query (MonadQuery)
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

data Timeout = Timeout deriving (Show)

instance Pretty Timeout where
    pretty Timeout = "Job killed by timeout"

instance Exception Timeout where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

nonBlockingLogHandle :: (MonadIO m, MonadLogger m) => Handle -> m ()
nonBlockingLogHandle hnd = do
    initial <- liftIO $ BS.hGetNonBlocking hnd 4096
    unless (BS.null initial) $ do
        fullOutput <- liftIO $ go (BS.byteString initial)
        Log.logWithoutLoc "Process#Error" Log.LevelDebug fullOutput
  where
    go :: BS.Builder -> IO LBS.ByteString
    go start = do
        rest <- BS.hGetNonBlocking hnd 4096
        if BS.null rest
           then return $ BS.toLazyByteString start
           else go (start <> BS.byteString rest)

data Process =
  Process
  { inHandle :: Handle
  , outHandle :: Handle
  , errHandle :: Handle
  , procHandle :: ProcessHandle
  , procId :: Pid
  , procToException :: ExitCode -> UnexpectedTermination
  }

getJobTimeOut :: MonadIO m => m [String]
getJobTimeOut = liftIO $ do
    localTime <- Time.zonedTimeToLocalTime <$> Time.getZonedTime
    let dayHour = Time.todHour $ Time.localTimeOfDay localTime
    return $ case dayOfWeek (Time.localDay localTime) of
        Sunday -> timeoutFlag 8
        Saturday -> timeoutFlag 8
        _ | dayHour > 20 -> timeoutFlag 8
          | dayHour < 8 -> timeoutFlag $ 8 - (dayHour + 1)
          | otherwise -> []
  where
    timeoutFlag :: Int -> [String]
    timeoutFlag h | h > 0     = ["-t", show h ++ ":00:00"]
                  | otherwise = []

type LogFun = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

withProcessPool
    :: (MonadLoggerIO m, MonadResource m, MonadQuery m)
    => Int -> Platform -> (Pool Process -> m a) -> m a
withProcessPool n Platform{platformName,platformFlags} f = do
    hostName <- liftIO getHostName
    logFun <- Log.askLoggerIO
    makeRunnerProc <- runnerCreator
    (releaseKey, pool) <- allocate
        (createProcessPool logFun hostName makeRunnerProc)
        destroyProcessPool

    f pool <* release releaseKey
  where
    createProcessPool
        :: LogFun
        -> String
        -> ([String] -> IO CreateProcess)
        -> IO (Pool Process)
    createProcessPool logFun hostName makeRunnerProc = Pool.createPool
            (unLog $ allocateProcess makeRunnerProc)
            (unLog . destroyProcess hostName)
            1
            3153600000
            n
      where
        unLog act = Log.runLoggingT act logFun

    destroyProcessPool :: Pool Process -> IO ()
    destroyProcessPool = liftIO . Pool.destroyAllResources

    customRunner :: Text -> [String] -> IO CreateProcess
    customRunner txtCmd args = return . Proc.proc cmd $ runnerArgs ++ args
      where
        cmd = T.unpack txtCmd
        runnerArgs = flags ++ ["--"]
        flags = map T.unpack $ case platformFlags of
            Nothing -> [platformName]
            Just t -> T.splitOn " " t

    defaultRunner :: [String] -> IO CreateProcess
    defaultRunner args = do
        timeout <- getJobTimeOut
        return . Proc.proc "srun" $ timeout ++ runnerArgs ++ args
      where
        runnerArgs = ["-Q", "--gres=gpu:1"] ++ flags ++ ["--"]
        flags = map T.unpack $ case platformFlags of
            Nothing -> ["-C", platformName]
            Just t -> T.splitOn " " t

    runnerCreator :: MonadQuery m => m ([String] -> IO CreateProcess)
    runnerCreator = do
        result <- getGlobalVar RunCommand
        case result of
            Just cmd -> return $ customRunner cmd
            Nothing -> return $ defaultRunner

    allocateProcess :: ([String] -> IO CreateProcess) -> LoggingT IO Process
    allocateProcess createRunnerProc = do
        exePath <- getKernelExecutable
        libPath <- getKernelLibPath
        proc@Process{procId,errHandle} <- liftIO $ do
            runnerProc <- createRunnerProc [exePath, "-L", libPath, "-W", "-S"]

            let p = runnerProc
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
                procToException = unexpectedTermination p

            (Just inHandle, Just outHandle, Just errHandle, procHandle) <-
                Proc.createProcess p

            System.hSetBuffering inHandle LineBuffering
            System.hSetBuffering outHandle LineBuffering
            System.hSetBuffering errHandle LineBuffering
            Just procId <- Proc.getPid procHandle

            return Process{..}

        nonBlockingLogHandle errHandle
        proc <$ logDebugNS "Process#Start" (showText procId)

    destroyProcess :: String -> Process -> LoggingT IO ()
    destroyProcess hostName Process{..} = do
        uninterruptibleMask_ $ do
            err <- liftIO $ do
                Proc.getPid procHandle >>= mapM_ (signalProcess sigKILL)
                System.hClose inHandle
                System.hClose outHandle
                Proc.waitForProcess procHandle
                T.hGetContents errHandle <* System.hClose errHandle
            logDebugNS "Process#ExitError" err
            tryRemoveFile $ "kernel-runner.0" <.> show procId <.> hostName
            tryRemoveFile $ ".PRUN_ENVIRONMENT" <.> show procId <.> hostName
        logDebugNS "Process#End" $ showText procId

    tryRemoveFile :: MonadIO m => FilePath -> m ()
    tryRemoveFile path = liftIO $
        void (try $ removeFile path :: IO (Either SomeException ()))

checkProcess :: (MonadIO m, MonadLogger m, MonadThrow m) => Process -> m ()
checkProcess Process{..} = do
    result <- liftIO $ Proc.getProcessExitCode procHandle
    case result of
        Just (ExitFailure 2) -> logThrowM Timeout
        Just code -> logThrowM $ procToException code
        Nothing -> return ()
    liftIO $ do
        System.hIsReadable outHandle >>= guard
        System.hIsWritable inHandle >>= guard

withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool f = withAcquire (mkAcquireType alloc clean) $ f . fst
  where
    alloc = Pool.takeResource pool
    clean (res, localPool) ty = case ty of
        ReleaseException -> Pool.destroyResource pool localPool res
        _                -> Pool.putResource localPool res

processJobsParallel
    :: ( MonadLoggerIO m
       , MonadQuery m
       , MonadMask m
       , MonadUnliftIO m
       )
    => Int -> Platform -> ConduitT (Job a) (Result a) m ()
processJobsParallel numNodes platform = withProcessPool numNodes platform $
    processJobsParallelWithSharedPool numNodes

processJobsParallelWithSharedPool
    :: ( MonadLoggerIO m
       , MonadQuery m
       , MonadMask m
       , MonadUnliftIO m
       )
    => Int -> Pool Process -> ConduitT (Job a) (Result a) m ()
processJobsParallelWithSharedPool numNodes procPool =
    parMapM taskHandler numNodes $ \Job{..} -> do

        let fileStem = T.unpack jobLabel
            outputFile = fileStem <.> "output"
            timingFile = fileStem <.> "timings"
            logFile = fileStem <.> "log"

            handleErrors act = act `onError` do
                logErrorN ("Failed: " <> jobCommand)
                tryRemoveFile $ outputFile
                tryRemoveFile $ timingFile
                tryRemoveFile $ logFile

        withResource procPool $ \proc@Process{inHandle,outHandle,errHandle} -> do
            checkProcess proc

            handleErrors $ do
                logDebugNS "Process#Job#Start" jobCommand
                result <- liftIO $ do
                    T.hPutStrLn inHandle jobCommand
                    T.hGetLine outHandle

                let (label, commit) = case T.splitOn ":" result of
                        (version:rest) -> (T.concat rest, version)
                        [] -> ("", "")

                logDebugNS "Process#Job#End" jobCommand
                outputKey <- registerFile outputFile
                timingKey <- registerFile timingFile
                logKey <- if jobLogProperties
                             then Just <$> registerFile logFile
                             else return Nothing

                nonBlockingLogHandle errHandle

                return $ Result
                    { resultValue = jobValue
                    , resultVariant = jobVariant
                    , resultLabel = label
                    , resultAlgorithmVersion = CommitId commit
                    , resultOutput = outputKey
                    , resultTimings = timingKey
                    , resultPropLog = logKey
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

taskHandler :: MonadSql m => Handler m (Job a)
taskHandler = Handle handler
  where
    handler :: MonadSql m => Job a -> SomeException -> m Action
    handler Job{jobVariant} exc
      | Just Timeout <- fromException exc = return Retry
      | otherwise = do
          retries <- variantRetryCount <$> Sql.getJust jobVariant
          if retries >= 5
             then return Drop
             else Retry <$ Sql.update jobVariant [ VariantRetryCount +=. 1 ]
