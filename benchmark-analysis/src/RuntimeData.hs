{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module RuntimeData
    ( getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getCxxCompilerWrapper
    , getBarPlotScript
    , getHeatmapScript
    , getModelScript
    , getOutputChecker
    ) where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.Hash (Digest, SHA512, digestFromByteString)
import Crypto.Hash.Conduit (hashFile)
import Data.Bool (bool)
import qualified Data.ByteArray as ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Foldable (asum)
import qualified System.Directory as Dir
import System.FilePath ((</>))

import ProcessTools

import Exceptions
import Paths_benchmark_analysis (getDataFileName)

getKernelExecutableMaybe :: MonadIO m => m (Maybe FilePath)
getKernelExecutableMaybe = liftIO $ do
    exePath <- getDataFileName "runtime-data/kernel-runner"
    bool Nothing (Just exePath) <$> Dir.doesFileExist exePath

getKernelExecutable :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelExecutable = getKernelExecutableMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingCxxKernelRunner

getKernelLibPathMaybe :: MonadIO m => m (Maybe FilePath)
getKernelLibPathMaybe = liftIO $ do
    libPath <- getDataFileName "runtime-data/kernels"
    bool Nothing (Just libPath) <$> Dir.doesDirectoryExist libPath

getKernelLibPath :: (MonadIO m, MonadLogger m, MonadThrow m) => m FilePath
getKernelLibPath = getKernelLibPathMaybe >>= maybe raiseError return
  where
    raiseError = logThrowM MissingKernelLibPath

findPython
    :: forall m
     . (MonadIO m, MonadLogger m, MonadMask m) => m FilePath
findPython = do
    mResult <- runMaybeT $ asum
        [ tryPythonVersion "python3.7"
        , tryPythonVersion "python3.6"
        , do
            logWarnN "Unable to find 'python3.6' or 'python3.7' executable, \
                     \trying 'python3'!"
            tryPythonVersion "python3"
        , do
            logWarnN "Unable to find 'python3' executable, trying 'python'!"
            tryPythonVersion "python"
        ]
    case mResult of
        Just r -> return r
        Nothing -> logThrowM MissingPython
  where
    tryPythonVersion :: MonadIO m => String -> MaybeT m FilePath
    tryPythonVersion python = MaybeT . liftIO $ Dir.findExecutable python

checkVirtualEnv
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => FilePath -> FilePath -> m ()
checkVirtualEnv virtualEnv requirements = do
    (reqHash :: Digest SHA512) <- hashFile requirements
    virtualVersion <- liftIO . Catch.try $ BS.readFile initialisedFile
    case virtualVersion of
        Right bs | digestFromByteString bs == Just reqHash -> return ()
        Right _ -> do
            logInfoN $ "Removing out of date virtualenv"
            liftIO $ Dir.removeDirectoryRecursive virtualEnv
            initVirtualEnv reqHash
        Left (SomeException _) -> initVirtualEnv reqHash
  where
    initialisedFile :: FilePath
    initialisedFile = virtualEnv </> "initialised"

    initVirtualEnv
        :: (MonadIO m, MonadLogger m, MonadMask m) => Digest SHA512 -> m ()
    initVirtualEnv reqHash = do
        logInfoN $ "Creating virtualenv"
        pythonExe <- findPython
        runProcess_ pythonExe ["-m", "venv", virtualEnv]

        logInfoN $ "Initialising virtualenv"
        pipExe <- liftIO $ getDataFileName "runtime-data/virtualenv/bin/pip3"
        runProcess_ pipExe ["install", "--upgrade", "pip"]
        runProcess_ pipExe ["install", "wheel"]
        runProcess_ pipExe ["install", "-r", requirements]
        liftIO $ BS.writeFile initialisedFile (ByteArray.convert reqHash)
            `Catch.onError` tryRemoveFile initialisedFile

    tryRemoveFile :: FilePath -> IO (Either SomeException ())
    tryRemoveFile path = Catch.try $ Dir.removeFile path

getPythonScript
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => String -> [String] -> m CreateProcess
getPythonScript script args = do
    (virtualenv, requirements) <- liftIO $ do
        symlinkPath <- getDataFileName "runtime-data/virtualenv"
        virtualenv <- Dir.canonicalizePath symlinkPath

        requirements <- getDataFileName "runtime-data/requirements.txt"
        return (virtualenv, requirements)

    checkVirtualEnv virtualenv requirements

    liftIO $ do
        pythonPath <- getDataFileName $ "runtime-data/virtualenv/bin/python"
        scriptPath <- getDataFileName $ "runtime-data/scripts" </> script
        return $ proc pythonPath (scriptPath : args)

getCxxCompilerWrapper :: MonadIO m => FilePath -> m CreateProcess
getCxxCompilerWrapper outputFile = liftIO $ do
    exePath <- getDataFileName "runtime-data/cxx-run.sh"
    return $ proc exePath [outputFile]

getBarPlotScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getBarPlotScript = getPythonScript "bar-plot.py"

getHeatmapScript
    :: (MonadIO m, MonadLogger m, MonadMask m) => [String] -> m CreateProcess
getHeatmapScript = getPythonScript "heatmap.py"

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
