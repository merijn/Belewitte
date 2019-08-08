{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessPool
    ( Process(Process,inHandle,outHandle)
    , mapWithProcessPool
    ) where

import Control.Exception (SomeException, try, uninterruptibleMask_)
import Control.Monad (guard, void)
import Control.Monad.Logger
    (Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans.Resource (allocate, release)
import Data.Conduit (ConduitT)
import Data.Acquire
    (withAcquire, mkAcquireType, ReleaseType(ReleaseException))
import Data.List (intercalate)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import Data.Time.Calendar (DayOfWeek(Saturday,Sunday), dayOfWeek)
import Network.HostName (getHostName)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>))
import System.IO (BufferMode(LineBuffering), Handle)
import qualified System.IO as System
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess(..), ProcessHandle, Pid, StdStream(..))
import qualified System.Process as Proc

import BroadcastChan.Conduit
import Core
import Utils.Process (UnexpectedTermination, unexpectedTermination)
import RuntimeData (getKernelExecutable, getKernelLibPath)
import Schema
import Sql (MonadSql, (+=.))
import qualified Sql

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
        _ | dayHour > 18 -> timeoutFlag 8
          | dayHour < 8 -> timeoutFlag $ 8 - (dayHour + 1)
          | otherwise -> []
  where
    timeoutFlag :: Int -> [String]
    timeoutFlag h = ["-t", show h ++ ":00:00"]

type LogFun = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

withProcessPool
    :: (MonadLoggerIO m, MonadResource m)
    => Int -> Platform -> (Pool Process -> m a) -> m a
withProcessPool n (Platform name _) f = do
    hostName <- liftIO getHostName
    logFun <- Log.askLoggerIO
    (releaseKey, pool) <-
        allocate (createProcessPool logFun hostName) destroyProcessPool

    f pool <* release releaseKey
  where
    createProcessPool :: LogFun -> String -> IO (Pool Process)
    createProcessPool logFun hostName = Pool.createPool
            (unLog allocateProcess)
            (unLog . destroyProcess hostName)
            1
            3153600000
            n
      where
        unLog act = Log.runLoggingT act logFun

    destroyProcessPool :: Pool Process -> IO ()
    destroyProcessPool = liftIO . Pool.destroyAllResources

    allocateProcess :: LoggingT IO Process
    allocateProcess = do
        exePath <- getKernelExecutable
        libPath <- getKernelLibPath
        proc@Process{procId} <- liftIO $ do
            timeout <- getJobTimeOut
            let p = (Proc.shell (opts timeout exePath libPath))
                    { std_in = CreatePipe, std_out = CreatePipe }

                procToException = unexpectedTermination p

            (Just inHandle, Just outHandle, Nothing, procHandle) <-
                Proc.createProcess p

            System.hSetBuffering inHandle LineBuffering
            System.hSetBuffering outHandle LineBuffering
            Just procId <- Proc.getPid procHandle

            return Process{..}
        proc <$ logInfoN ("Started new process: " <> showText procId)
      where
        opts timeout exePath libPath = intercalate " " . ("srun":) $ timeout ++
            [ "-Q", "--gres=gpu:1", "-C ", T.unpack name, exePath
            , "-L", libPath, "-W", "-S"
            ]

    destroyProcess :: String -> Process -> LoggingT IO ()
    destroyProcess hostName Process{..} = do
        liftIO . uninterruptibleMask_ $ do
            Proc.getPid procHandle >>= mapM_ (signalProcess sigKILL)
            System.hClose inHandle
            System.hClose outHandle
            () <$ Proc.waitForProcess procHandle
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

withProcess
    :: (MonadLogger m, MonadThrow m, MonadUnliftIO m)
    => Pool Process -> (Process -> a -> m b) -> a -> m b
withProcess pool f x = withResource pool $ \process@Process{..} -> do
    checkProcess process
    f process x

mapWithProcessPool
    :: ( MonadLoggerIO m
       , MonadResource m
       , MonadSql m
       , MonadThrow m
       , MonadUnliftIO m
       )
    => Int
    -> Platform
    -> (Process -> (a, Key Variant, b, c, d) -> m e)
    -> ConduitT (a, Key Variant, b, c, d) e m ()
mapWithProcessPool numNodes platform f =
    withProcessPool numNodes platform $ \procPool ->
        parMapM taskHandler numNodes (withProcess procPool f)

taskHandler :: MonadSql m => Handler m (a, Key Variant, b, c, d)
taskHandler = Handle handler
  where
    handler
        :: MonadSql m => (a, Key Variant, b, c, d) -> SomeException -> m Action
    handler (_, varId, _, _, _) exc
      | Just Timeout <- fromException exc = return Retry
      | otherwise = do
          retries <- variantRetryCount <$> Sql.getJust varId
          if retries >= 5
             then return Drop
             else Retry <$ Sql.update varId [ VariantRetryCount +=. 1 ]
