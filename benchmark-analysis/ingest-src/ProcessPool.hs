{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessPool
    ( Job
    , Result(..)
    , Process(Process,inHandle,outHandle)
    , makeJob
    , processJobsParallel
    , processJobsParallelWithSharedPool
    , withProcessPool
    ) where

import Control.Monad (guard, void)
import Control.Monad.Catch (SomeException, onError, try, uninterruptibleMask_)
import Control.Monad.Logger
    (Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans.Resource (allocate, release)
import Data.Conduit (ConduitT)
import Data.Acquire (withAcquire, mkAcquireType, ReleaseType(ReleaseException))
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
import Utils.Process (UnexpectedTermination, unexpectedTermination)
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
    } deriving (Functor, Foldable, Traversable)

makeJob :: a -> Key Variant -> Maybe Text -> [Text] -> Job a
makeJob val variantId implName args = Job
    { jobValue = val
    , jobVariant = variantId
    , jobLabel = label
    , jobCommand = T.unwords $ label: args
    }
  where
    label = case implName of
        Nothing -> showSqlKey variantId
        Just name -> mconcat ["\"", showSqlKey variantId, " ", name, "\""]

data Result a = Result
    { resultValue :: a
    , resultVariant :: Key Variant
    , resultLabel :: Text
    , resultAlgorithmVersion :: Text
    } deriving (Functor, Foldable, Traversable)

data Timeout = Timeout deriving (Show)

instance Pretty Timeout where
    pretty Timeout = "Job killed by timeout"

instance Exception Timeout where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data Process =
  Process
  { inHandle :: Handle
  , outHandle :: Handle
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
    timeoutFlag h = ["-t", show h ++ ":00:00"]

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
        proc@Process{procId} <- liftIO $ do
            runnerProc <- createRunnerProc [exePath, "-L", libPath, "-W", "-S"]

            let p = runnerProc { std_in = CreatePipe, std_out = CreatePipe }
                procToException = unexpectedTermination p

            (Just inHandle, Just outHandle, Nothing, procHandle) <-
                Proc.createProcess p

            System.hSetBuffering inHandle LineBuffering
            System.hSetBuffering outHandle LineBuffering
            Just procId <- Proc.getPid procHandle

            return Process{..}
        proc <$ logInfoN ("Started new process: " <> showText procId)

    destroyProcess :: String -> Process -> LoggingT IO ()
    destroyProcess hostName Process{..} = do
        liftIO . uninterruptibleMask_ $ do
            Proc.getPid procHandle >>= mapM_ (signalProcess sigKILL)
            System.hClose inHandle
            System.hClose outHandle
            Proc.waitForProcess procHandle
            tryRemoveFile $ "kernel-runner.0" <.> show procId <.> hostName
            tryRemoveFile $ ".PRUN_ENVIRONMENT" <.> show procId <.> hostName
        logInfoN $ "Destroyed process: " <> showText procId

    tryRemoveFile :: FilePath -> IO ()
    tryRemoveFile path =
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
            handleErrors act = act `onError` do
                logErrorN ("Failed: " <> jobCommand)
                tryRemoveFile $ fileStem <.> "timings"
                tryRemoveFile $ fileStem <.> "output"
                tryRemoveFile $ fileStem <.> "log"

        withResource procPool $ \process@Process{inHandle,outHandle} -> do
            checkProcess process
            handleErrors $ do
                logInfoN $ "Running: " <> jobCommand
                result <- liftIO $ do
                    T.hPutStrLn inHandle jobCommand
                    T.hGetLine outHandle

                let (label, commit) = case T.splitOn ":" result of
                        (version:rest) -> (T.concat rest, version)
                        [] -> ("", "")

                logInfoN $ "Finished: " <> jobCommand
                return $ Result jobValue jobVariant label commit
  where
    checkNotExist :: SomeException -> Bool
    checkNotExist e = fromMaybe False $ isDoesNotExistError <$> fromException e

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
