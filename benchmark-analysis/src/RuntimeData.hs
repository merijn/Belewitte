{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module RuntimeData
    ( OutputDiff(..)
    , getKernelExecutableMaybe
    , getKernelExecutable
    , getKernelLibPathMaybe
    , getKernelLibPath
    , getCxxCompilerWrapper
    , getBarPlotScript
    , getHeatmapScript
    , getModelScript
    , getOutputChecker
    , renderOutputDiff
    ) where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.Hash (Digest, SHA512, digestFromByteString)
import Crypto.Hash.Conduit (hashFile)
import Data.Attoparsec.Text
import Data.Bool (bool)
import qualified Data.ByteArray as ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Foldable (asum)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
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

data OutputDiff = OutputDiff
    { maxAbsDiff :: {-# UNPACK #-} !Double
    , maxRelDiff :: {-# UNPACK #-} !Double
    , diffCount :: {-# UNPACK #-} !Int
    }

instance Semigroup OutputDiff where
    diff1 <> diff2 = OutputDiff
      { maxAbsDiff = (max `on` maxAbsDiff) diff1 diff2
      , maxRelDiff = (max `on` maxRelDiff) diff1 diff2
      , diffCount = diffCount diff1 + diffCount diff2
      }

instance Monoid OutputDiff where
    mempty = OutputDiff 0 0 0

showText :: Show a => a -> Text
showText = T.pack . show

renderOutputDiff :: OutputDiff -> Text
renderOutputDiff OutputDiff{..} = T.unlines
    [ "Max abs error: " <> showText maxAbsDiff
    , "Max rel error: " <> showText maxRelDiff <> " ("
        <> T.pack (showFFloat (Just 2) (100 * maxRelDiff) "%)")
    , "Error count: " <> showText diffCount
    ]

outputDiff :: Parser OutputDiff
outputDiff = do
    option () $ do
        string "Max absolute error: " *> double *> endOfLine

    option () $ do
        string "Max relative error: "
        double *> string " (" *> double *> string "%)" *> endOfLine

    absDiff <- string "Max abs diff: " *> double <* endOfLine
    relDiff <- string "Max rel diff: " *> double <* string " ("
        <* double <* string "%)" <* endOfLine

    errCount <- string "Error count: " *> decimal <* endOfLine
    endOfInput

    pure $ OutputDiff
      { maxAbsDiff = absDiff
      , maxRelDiff = relDiff
      , diffCount = errCount
      }

getOutputChecker
    :: (MonadIO m, MonadLogger m, MonadMask m, MonadIO n)
    => n (FilePath -> FilePath -> m (Bool, OutputDiff))
getOutputChecker = liftIO $ do
    exePath <- getDataFileName "runtime-data/numdiff.awk"
    return $ \file1 file2 -> do
        (exitCode, txt) <- readStdout $
            proc exePath ["-r","0.025","-e",file1,file2]

        case parseOnly outputDiff txt of
            Left exc -> logThrowM $ OutputDiffUnparseable exc
            Right diff@OutputDiff{..} -> return $ case exitCode of
                ExitSuccess -> (True, diff)
                ExitFailure _
                    | maxRelDiff < 0.05 && diffCount <= 2 -> (True, diff)
                    | otherwise -> (False, diff)
