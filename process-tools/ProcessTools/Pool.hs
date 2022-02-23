{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessTools.Pool
    ( Pool
    , Process
    , Runner
    , Task(..)
    , Timeout(..)
    , customRunner
    , defaultRunner
    , defaultGpuRunner
    , withProcessPool
    , processTasksParallel
    , processTasksParallelWithSharedPool
    ) where

import Control.Monad (guard, unless, void)
import Control.Monad.Trans (lift)
import Control.Monad.Catch (Exception(..), MonadMask, MonadThrow, SomeException)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Unlift (MonadIO(liftIO), MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, LoggingT)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.Acquire (withAcquire, mkAcquireType, ReleaseType(ReleaseException))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import Data.Conduit (ConduitT)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
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
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess(..), ProcessHandle, Pid, StdStream(..))
import qualified System.Process as Proc

import Data.Text.Prettyprint.Doc (Pretty(pretty))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty

import BroadcastChan.Conduit
import ProcessTools (UnexpectedTermination, unexpectedTermination)

showText :: Show a => a -> Text
showText = T.pack . show

logThrowM :: (Exception e, MonadLogger m, MonadThrow m, Pretty e) => e -> m r
logThrowM exc = do
    Log.logErrorN . Pretty.renderStrict . layoutException $ exc
    Catch.throwM exc
  where
    layoutException :: Pretty e => e -> Pretty.SimpleDocStream a
    layoutException = Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

data Task m a b = Task
    { taskGenJob :: a -> Text
    , taskHandleErrors :: forall r . a -> m r -> m r
    , taskGenResult :: a -> Text -> m b
    , taskHandler :: Handler m a
    }

data Timeout = Timeout deriving (Show)

instance Pretty Timeout where
    pretty Timeout = "Job killed by timeout"

instance Exception Timeout where
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

newtype Runner = Runner { run :: IO CreateProcess }

customRunner :: Text -> [String] -> [String] -> Runner
customRunner runnerCmd platformFlags args = Runner . return $
    Proc.proc cmd runnerArgs
  where
    cmd = T.unpack runnerCmd
    runnerArgs = platformFlags ++ ["--"] ++ args

defaultRunner :: [String] -> Runner
defaultRunner args = Runner $ do
    timeout <- getJobTimeOut
    return . Proc.proc "srun" $ timeout ++ ["-Q", "--"] ++ args

defaultGpuRunner :: [String] -> [String] -> Runner
defaultGpuRunner platformFlags args = Runner $ do
    timeout <- getJobTimeOut
    return . Proc.proc "srun" $ timeout ++ runnerArgs ++ args
  where
    runnerArgs = ["-Q", "--gres=gpu:1"] ++ platformFlags ++ ["--"]

type LogFun = Log.Loc -> Log.LogSource -> Log.LogLevel -> Log.LogStr -> IO ()

withProcessPool
    :: (MonadLoggerIO m, MonadResource m)
    => Int -> m Runner -> (Pool Process -> m a) -> m a
withProcessPool n runnerCreator f = do
    hostName <- liftIO getHostName
    logFun <- Log.askLoggerIO
    runner <- runnerCreator
    (releaseKey, pool) <- allocate
        (createProcessPool logFun hostName runner)
        destroyProcessPool

    f pool <* release releaseKey
  where
    createProcessPool
        :: LogFun
        -> String
        -> Runner
        -> IO (Pool Process)
    createProcessPool logFun hostName runner = Pool.createPool
            (unLog $ allocateProcess runner)
            (unLog . destroyProcess hostName)
            1
            3153600000
            n
      where
        unLog act = Log.runLoggingT act logFun

    destroyProcessPool :: Pool Process -> IO ()
    destroyProcessPool = liftIO . Pool.destroyAllResources

    allocateProcess :: Runner -> LoggingT IO Process
    allocateProcess runner = do
        proc@Process{procId,errHandle} <- liftIO $ do
            runnerProc <- run runner
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
        proc <$ Log.logDebugNS "Process#Start" (showText procId)

    destroyProcess :: String -> Process -> LoggingT IO ()
    destroyProcess hostName Process{..} = do
        Catch.uninterruptibleMask_ $ do
            err <- liftIO $ do
                Proc.getPid procHandle >>= mapM_ (signalProcess sigKILL)
                System.hClose inHandle
                System.hClose outHandle
                Proc.waitForProcess procHandle
                T.hGetContents errHandle <* System.hClose errHandle
            Log.logDebugNS "Process#ExitError" err
            tryRemoveFile $ "kernel-runner.0" <.> show procId <.> hostName
            tryRemoveFile $ ".PRUN_ENVIRONMENT" <.> show procId <.> hostName
        Log.logDebugNS "Process#End" $ showText procId

    tryRemoveFile :: MonadIO m => FilePath -> m ()
    tryRemoveFile path = liftIO $
        void (Catch.try $ removeFile path :: IO (Either SomeException ()))

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

processTasksParallel
    :: (MonadLoggerIO m, MonadMask m, MonadResource m, MonadUnliftIO m)
    => Task m a b -> Int -> m Runner -> ConduitT a b m ()
processTasksParallel task numNodes getRunner =
  withProcessPool numNodes (lift getRunner) $
    processTasksParallelWithSharedPool task numNodes

processTasksParallelWithSharedPool
    :: (MonadLoggerIO m, MonadMask m, MonadResource m, MonadUnliftIO m)
    => Task m a b -> Int -> Pool Process -> ConduitT a b m ()
processTasksParallelWithSharedPool Task{..} numNodes procPool =
  parMapM taskHandler numNodes $ \val -> do
    let jobCommand = taskGenJob val

    withResource procPool $ \proc@Process{inHandle,outHandle,errHandle} -> do
        checkProcess proc

        let logErrors act = act `Catch.onError` nonBlockingLogHandle errHandle
        taskHandleErrors val $ do
            Log.logDebugNS "Process#Job#Start" jobCommand

            output <- logErrors . liftIO $ do
                T.hPutStrLn inHandle jobCommand
                T.hGetLine outHandle

            Log.logDebugNS "Process#Job#End" jobCommand

            taskGenResult val output <* nonBlockingLogHandle errHandle
