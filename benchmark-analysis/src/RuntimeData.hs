{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
module RuntimeData
    ( getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getBarPlotScript
    , getModelScript
    ) where

import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (bool)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Exceptions
import Paths_benchmark_analysis (getDataFileName)
import Utils.Process

getKernelExecutableMaybe :: MonadIO m => m (Maybe FilePath)
getKernelExecutableMaybe = liftIO $ do
    exePath <- getDataFileName "runtime-data/kernel-runner"
    bool Nothing (Just exePath) <$> doesFileExist exePath

getKernelExecutable :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelExecutable = getKernelExecutableMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingCxxKernelRunner

getKernelLibPathMaybe :: MonadIO m => m (Maybe FilePath)
getKernelLibPathMaybe = liftIO $ do
    libPath <- getDataFileName "runtime-data/kernels"
    bool Nothing (Just libPath) <$> doesFileExist libPath

getKernelLibPath :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelLibPath = getKernelLibPathMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingKernelLibPath

getPythonScript
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => String -> [String] -> m CreateProcess
getPythonScript script args = do
    (virtualenv, requirements) <- liftIO $ do
        symlinkPath <- getDataFileName "runtime-data/virtualenv"
        virtualenv <- canonicalizePath symlinkPath

        requirements <- getDataFileName "runtime-data/requirements.txt"
        return (virtualenv, requirements)

    exists <- liftIO $ doesDirectoryExist virtualenv
    unless exists $ do
        logInfoN $ "Initialising virtualenv"
        runProcess "virtualenv" [virtualenv]
        pipExe <- liftIO $ getDataFileName "runtime-data/virtualenv/bin/pip"
        runProcess pipExe ["install", "--upgrade", "pip"]
        runProcess pipExe ["install", "-r", requirements]

    liftIO $ do
        pythonPath <- getDataFileName $ "runtime-data/virtualenv/bin/python"
        scriptPath <- getDataFileName $ "runtime-data/scripts" </> script
        return $ proc pythonPath (scriptPath : args)

getBarPlotScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getBarPlotScript = getPythonScript "bar-plot.py"

getModelScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getModelScript = getPythonScript "model.py"
