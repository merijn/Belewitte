{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module RuntimeData
    ( getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getBarPlotScript
    , getModelScript
    , getOutputChecker
    ) where

import Control.Monad (unless)
import Control.Monad.Catch (MonadMask, MonadThrow, mask_)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (bool)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import qualified System.Posix.Files as Posix
import System.Posix.IO (OpenMode(WriteOnly))
import qualified System.Posix.IO as Posix
import System.Posix.Types (Fd)

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
    bool Nothing (Just libPath) <$> doesDirectoryExist libPath

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

    exists <- liftIO . doesFileExist $ virtualenv </> "bin" </> "python2.7"

    unless exists $ do
        logInfoN $ "Creating virtualenv"
        runProcess_ "virtualenv-2.7" [virtualenv]

    let initialisedFile = virtualenv </> "initialised"

    virtualenvInitialised <- liftIO $ doesFileExist $ initialisedFile
    unless virtualenvInitialised $ do
        logInfoN $ "Initialising virtualenv"
        pipExe <- liftIO $ getDataFileName "runtime-data/virtualenv/bin/pip"
        runProcess_ pipExe ["install", "--upgrade", "pip"]
        runProcess_ pipExe ["install", "-r", requirements]
        mask_ . liftIO $ touchFile initialisedFile >>= Posix.closeFd

    liftIO $ do
        pythonPath <- getDataFileName $ "runtime-data/virtualenv/bin/python2.7"
        scriptPath <- getDataFileName $ "runtime-data/scripts" </> script
        return $ proc pythonPath (scriptPath : args)
  where
    touchFile :: MonadIO m => FilePath -> m Fd
    touchFile path = liftIO $
        Posix.openFd path WriteOnly (Just Posix.stdFileMode) flags
      where
        flags = Posix.defaultFileFlags{Posix.exclusive = True}

getBarPlotScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getBarPlotScript = getPythonScript "bar-plot.py"

getModelScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getModelScript = getPythonScript "model.py"

getOutputChecker
    :: (MonadIO m, MonadLogger m, MonadMask m, MonadIO n)
    => n (FilePath -> FilePath -> m Bool)
getOutputChecker = liftIO $ do
    exePath <- getDataFileName "runtime-data/numdiff.awk"
    return $ \file1 file2 -> do
        runProcess exePath [file1,file2] >>= \case
            ExitSuccess -> return True
            _ -> return False
