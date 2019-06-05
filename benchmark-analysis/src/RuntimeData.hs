{-# LANGUAGE MonadFailDesugaring #-}
module RuntimeData
    ( getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getBarPlotScript
    , getModelScript
    ) where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (bool)
import System.Directory (doesFileExist)
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
getPythonScript script args = liftIO $ do
    scriptPath <- getDataFileName $ "runtime-data/scripts" </> script
    return $ proc scriptPath args

getBarPlotScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getBarPlotScript = getPythonScript "bar-plot.py"

getModelScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getModelScript = getPythonScript "model.py"
