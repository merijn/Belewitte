{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FormattedOutput (renderColumns, renderOutput, outputSink) where

import Control.Monad (join, unless)
import Control.Monad.Catch (catch, throwM, try)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (release)
import Data.Acquire (ReleaseType(..), allocateAcquire, mkAcquireType)
import Data.Conduit (ConduitT, Void, (.|), runConduit, yield)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process
    (CreateProcess, Inherited(..), ProcessExitedUnsuccessfully(..))
import qualified Data.Conduit.Process as Process
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (persistFieldDef)
import Database.Persist.Types (fieldHaskell, unHaskellName)
import GHC.IO.Exception (IOException, IOErrorType(ResourceVanished))
import Lens.Micro.Extras (view)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hIsTerminalDevice, stdout)
import System.IO.Error (ioeGetErrorType)

import Core
import Pretty.Columns
import Query (MonadQuery)
import Sql
import Utils.Process (UnexpectedTermination(..))

columnName :: PersistEntity r => EntityField r a -> Text
columnName = unHaskellName . fieldHaskell . persistFieldDef

queryColumnInfo
    :: (PrettyColumns a, MonadQuery m)
    => ColumnInfo a -> m (ColumnInfo a, (Avg, Max))
queryColumnInfo ColSeparator = return (ColSeparator, (Avg 1, Max 3))
queryColumnInfo col@(ColInfo field _) =
    annotateColumn <$> Sql.getFieldLength field
  where
    columnSize = Max . T.length . columnName $ field
    annotateColumn (avgVal, maxVal) = (col, (avgVal, max columnSize maxVal))

columnFormatter
    :: (MonadQuery m, PrettyColumns a) => m (Text, Entity a -> Text)
columnFormatter = do
    annotatedColumns <- traverse queryColumnInfo prettyColumnInfo
    let renderEntity = foldMap padColumn annotatedColumns
        headerText = foldMap header annotatedColumns
    return (headerText, renderEntity)
  where
    padText :: Int -> Text -> Text
    padText n input = input <> T.replicate (n - T.length input) " "

    header :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Text
    header (ColSeparator, (_, Max n)) = T.replicate n " "
    header (ColInfo field _, (_, Max n)) = padText n $ columnName field

    padColumn
        :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Entity a -> Text
    padColumn (ColSeparator, (_, Max n)) _ = T.replicate n " "
    padColumn (ColInfo f toText, (_, Max n)) val = padText n col
      where
        col = toText . view (Sql.fieldLens f) $ val

renderColumns :: PrettyColumns a => [Filter a] -> [SelectOpt a] -> SqlM ()
renderColumns filts order = do
    (header, f) <- columnFormatter
    let columnSource = do
            yield header
            Sql.selectSource filts order .| C.map f
    res <- try . runConduit $ columnSource .| C.unlines .| outputSink
    case res of
        Right _ -> return ()
        Left (ioeGetErrorType -> ResourceVanished) -> return ()
        Left e -> throwM e

renderOutput :: ConduitT () Text SqlM () -> SqlM ()
renderOutput producer = do
    res <- try . runConduit $ producer .| outputSink
    case res of
        Right _ -> return ()
        Left (ioeGetErrorType -> ResourceVanished) -> return ()
        Left e -> throwM e

outputSink :: ConduitT Text Void SqlM ()
outputSink = do
    usePager <- lift pagerSetting
    isTerm <- liftIO $ hIsTerminalDevice stdout
    case usePager of
        Never -> unpagedOutput
        Auto | isTerm -> outputPager
             | otherwise -> unpagedOutput
        Always -> outputPager
  where
    unpagedOutput :: ConduitT Text Void SqlM ()
    unpagedOutput = C.encodeUtf8 .| C.sinkHandle stdout

    fallback :: ConduitT Text Void SqlM ()
    fallback = do
      logWarnN "No pager program found! This shouldn't happen on sane systems!"
      unpagedOutput

    outputPager :: ConduitT Text Void SqlM ()
    outputPager = join $ maybe fallback pagerConduit <$> findPager

findPager :: MonadIO m => m (Maybe CreateProcess)
findPager = runMaybeT $ asum
    [ Process.proc <$> find "less" <*> pure ["-FRSX"]
    , Process.proc <$> find "more" <*> pure []
    ]
  where
    find = MaybeT . liftIO . findExecutable

pagerConduit :: CreateProcess -> ConduitT Text Void SqlM ()
pagerConduit process = do
    (key, ((inStream, closeIn), Inherited, Inherited, sph)) <-
        allocateAcquire acquireProcess

    C.encodeUtf8 .| inStream >> closeIn
    ec <- Process.waitForStreamingProcess sph <* release key
    unless (ec == ExitSuccess) $ do
        let exc = ProcessExitedUnsuccessfully process ec
        logThrowM $ UnexpectedTermination exc
  where
    acquireProcess = mkAcquireType (Process.streamingProcess process) cleanup

    stdinClosed :: IOException -> IO ()
    stdinClosed (ioeGetErrorType -> ResourceVanished) = return ()
    stdinClosed exc = throwM exc

    cleanup (_, _, _, sph) ReleaseException = do
        Process.terminateProcess $ Process.streamingProcessHandleRaw sph
        Process.waitForStreamingProcess sph
        Process.closeStreamingProcessHandle sph `catch` stdinClosed

    cleanup (_, _, _, sph) _ = do
        Process.closeStreamingProcessHandle sph `catch` stdinClosed
