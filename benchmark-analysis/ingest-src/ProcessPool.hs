{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessPool
    ( Pool
    , Process(..)
    , Timeout
    , withProcessPool
    , withProcess
    ) where

import Control.Exception (Exception)
import Control.Monad (guard)
import Control.Monad.Catch (MonadMask, bracket, throwM, uninterruptibleMask_)
import Data.Acquire (mkAcquireType, withAcquire, ReleaseType(ReleaseException))
import Data.List (intercalate)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import Data.Time.Calendar (DayOfWeek(Saturday,Sunday), dayOfWeek)
import System.Exit (ExitCode(ExitFailure))
import System.IO (BufferMode(LineBuffering), Handle)
import qualified System.IO as System
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess(..), ProcessHandle, Pid, StdStream(..))
import qualified System.Process as Proc

import Core
import Paths_benchmark_analysis (getDataFileName)
import Schema

data Timeout = Timeout deriving (Show)
instance Exception Timeout

data Process =
  Process
  { inHandle :: Handle
  , outHandle :: Handle
  , procHandle :: ProcessHandle
  , procId :: Pid
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

withProcessPool
    :: forall a m . (MonadLogger m, MonadMask m, MonadUnliftIO m)
    => Int -> GPU -> (Pool Process -> m a) -> m a
withProcessPool n (GPU name _) = bracket createProcessPool destroyProcessPool
  where
    createProcessPool :: MonadIO m => m (Pool Process)
    createProcessPool = withUnliftIO $ \(UnliftIO runInIO) -> Pool.createPool
        (runInIO allocateProcess)
        (runInIO . destroyProcess)
        1
        3153600000
        n

    destroyProcessPool :: MonadIO m => Pool Process -> m ()
    destroyProcessPool = liftIO . Pool.destroyAllResources

    allocateProcess :: (MonadLogger m, MonadUnliftIO m) => m Process
    allocateProcess = do
        proc@Process{procId} <- liftIO $ do
            timeout <- getJobTimeOut
            exePath <- getDataFileName "runtime-data/main"
            libPath <- getDataFileName "runtime-data/kernels"
            let p = (Proc.shell (opts timeout exePath libPath))
                    { std_in = CreatePipe, std_out = CreatePipe }
            (Just inHandle, Just outHandle, Nothing, procHandle) <-
                Proc.createProcess p

            System.hSetBuffering inHandle LineBuffering
            System.hSetBuffering outHandle LineBuffering
            Just procId <- Proc.getPid procHandle
            return Process{..}
        proc <$ logInfoN ("Started new process: " <> showText procId)
      where
        opts timeout exePath libPath = intercalate " " . ("prun":) $ timeout ++
            [ "-np", "1" , "-native","\"-C " ++ T.unpack name ++"\""
            , "CUDA_VISIBLE_DEVICES=\"0,1,2,3,4,5,6,7,8,9,10\""
            , exePath, "-L", libPath, "-W", "-S"
            ]

    destroyProcess :: (MonadLogger m, MonadUnliftIO m) => Process -> m ()
    destroyProcess Process{..} = do
        liftIO . uninterruptibleMask_ $ do
            Proc.getPid procHandle >>= mapM_ (signalProcess sigKILL)
            System.hClose inHandle
            System.hClose outHandle
            () <$ Proc.waitForProcess procHandle
        logInfoN $ "Destroyed process: " <> showText procId

checkProcess :: MonadIO m => Process -> m ()
checkProcess Process{..} = liftIO $ do
    result <- Proc.getProcessExitCode procHandle
    case result of
        Just (ExitFailure 2) -> throwM Timeout
        Just code -> throwM . Error $
            "Process died with code: " <> showText code
        Nothing -> return ()
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
    :: (Show a, MonadMask m, MonadLogger m, MonadUnliftIO m)
    => Pool Process -> (Process -> a -> m b) -> a -> m b
withProcess pool f x = withResource pool $ \process@Process{..} -> do
    checkProcess process
    f process x
