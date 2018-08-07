{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProcessPool(Pool, Process(..), withProcessPool, withProcess) where

import Control.Monad (guard)
import Control.Monad.Catch
    (MonadMask, bracket, throwM, uninterruptibleMask_)
import Data.List (intercalate)
import Data.Pool (Pool, createPool, destroyAllResources, withResource)
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import Data.Time.Calendar (DayOfWeek(Saturday,Sunday), dayOfWeek)
import System.IO
    ( BufferMode(LineBuffering), Handle, hClose, hIsReadable
    , hIsWritable, hSetBuffering)
import System.Process
    ( CreateProcess(std_in, std_out), ProcessHandle
    , StdStream(CreatePipe), createProcess, getProcessExitCode, shell
    , terminateProcess, waitForProcess)

import Schema

data Process =
  Process
  { inHandle :: Handle
  , outHandle :: Handle
  , procHandle :: ProcessHandle
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
    :: (MonadMask m, MonadIO m) => Int -> GPU -> (Pool Process -> m a) -> m a
withProcessPool n (GPU name _) = bracket createProcessPool destroyProcessPool
  where
    createProcessPool :: MonadIO m => m (Pool Process)
    createProcessPool = liftIO $
        createPool allocateProcess destroyProcess 1 3153600000 n

    destroyProcessPool :: MonadIO m => Pool Process -> m ()
    destroyProcessPool = liftIO . destroyAllResources

    allocateProcess :: MonadIO m => m Process
    allocateProcess = liftIO $ do
        timeout <- getJobTimeOut
        let p = (shell (opts timeout)) { std_in = CreatePipe, std_out = CreatePipe }
        (Just inHandle, Just outHandle, Nothing, procHandle) <- createProcess p
        hSetBuffering inHandle LineBuffering
        hSetBuffering outHandle LineBuffering
        return Process{..}
      where
        opts timeout = intercalate " " . ("prun":) $ timeout ++
            [ "-np", "1" , "-native","\"-C " ++ T.unpack name ++"\""
            , "CUDA_VISIBLE_DEVICES=\"0,1,2,3,4,5,6,7,8,9,10\""
            , "./main", "-W", "-S"
            ]

    destroyProcess :: MonadIO m => Process -> m ()
    destroyProcess Process{..} = liftIO . uninterruptibleMask_ $ do
        terminateProcess procHandle
        waitForProcess procHandle
        hClose inHandle
        hClose outHandle

checkProcess :: Process -> IO ()
checkProcess Process{..}= do
    result <- getProcessExitCode procHandle
    case result of
        Just _ -> throwM $ Error "gah!"
        Nothing -> return ()
    hIsReadable outHandle >>= guard
    hIsWritable inHandle >>= guard

withProcess :: Pool Process -> (Process -> a -> IO b) -> a -> IO b
withProcess pool f x = withResource pool $ \process@Process{..} -> do
    checkProcess process
    f process x
