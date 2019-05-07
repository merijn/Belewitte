{-# LANGUAGE MonadFailDesugaring #-}
module RuntimeData
    ( getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getBarPlotScript
    , getModelScript
    ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (bool)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Exceptions

import Paths_benchmark_analysis (getDataFileName)

getKernelExecutableMaybe :: MonadIO m => m (Maybe FilePath)
getKernelExecutableMaybe = liftIO $ do
    exePath <- getDataFileName "runtime-data/main"
    bool Nothing (Just exePath) <$> doesFileExist exePath

getKernelExecutable :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelExecutable = getKernelExecutableMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingCxxMain

getKernelLibPathMaybe :: MonadIO m => m (Maybe FilePath)
getKernelLibPathMaybe = liftIO $ do
    libPath <- getDataFileName "runtime-data/kernels"
    bool Nothing (Just libPath) <$> doesFileExist libPath

getKernelLibPath :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelLibPath = getKernelLibPathMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingKernelLibPath

getPythonScript
    :: (MonadIO m, MonadLogger m, MonadThrow m) => String -> m FilePath
getPythonScript script = do
    scriptPath <- liftIO . getDataFileName $ "runtime-data/scripts" </> script
    return scriptPath

getBarPlotScript :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getBarPlotScript = getPythonScript "bar-plot.py"

getModelScript :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getModelScript = getPythonScript "model.py"
