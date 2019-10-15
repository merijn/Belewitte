{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module FormattedOutput (renderColumns, renderOutput, outputSink) where

import Control.Monad (unless)
import Control.Monad.Catch (catch, throwM, try)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (release)
import Data.Acquire (ReleaseType(..), allocateAcquire, mkAcquireType)
import Data.Conduit (ConduitT, Void, (.|), runConduit, yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (isolate)
import Data.Conduit.Process (CreateProcess, Inherited(..))
import qualified Data.Conduit.Process as Process
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Class (persistFieldDef)
import Database.Persist.Types (fieldHaskell, unHaskellName)
import GHC.IO.Exception (IOException, IOErrorType(ResourceVanished))
import Lens.Micro.Extras (view)
import System.Console.Terminal.Size (hSize, height)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hIsTerminalDevice, stdout)
import System.IO.Error (ioeGetErrorType)

import Core
import Pretty.Columns
import Query (MonadQuery)
import Sql
import Utils.Process (unexpectedTermination)

columnName :: PersistEntity r => EntityField r a -> Text
columnName = unHaskellName . fieldHaskell . persistFieldDef

queryColumnInfo
    :: (PrettyColumns a, MonadQuery m)
    => ColumnInfo a -> m (ColumnInfo a, (Avg, Max))
queryColumnInfo col@(ColInfo field _) =
    annotateColumn <$> Sql.getFieldLength field
  where
    columnSize = Max . T.length . columnName $ field
    annotateColumn (avgVal, maxVal) = (col, (avgVal, max columnSize maxVal))

columnFormatter
    :: (MonadQuery m, PrettyColumns a) => m (Text, Entity a -> Text)
columnFormatter = do
    (startCol :| columns) <- traverse queryColumnInfo prettyColumnInfo
    let renderEntity = padColumn startCol <> foldMap sepPadColumn columns
        headerText = header startCol <> foldMap sepHeader columns
    return (headerText, renderEntity)
  where
    sep :: Text
    sep = "   "

    padText :: Int -> Text -> Text
    padText n input = input <> T.replicate (n - T.length input) " "

    header :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Text
    header (ColInfo field _, (_, Max n)) = padText n $ columnName field

    sepHeader :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Text
    sepHeader x = sep <> header x

    padColumn
        :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Entity a -> Text
    padColumn (ColInfo f toText, (_, Max n)) val = padText n col
      where
        col = toText . view (Sql.fieldLens f) $ val

    sepPadColumn
        :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Entity a -> Text
    sepPadColumn x y = sep <> padColumn x y

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
    outputPager = do
        maybeHeight <- liftIO $ fmap height <$> hSize stdout
        initialText <- case maybeHeight of
            Nothing -> return ""
            Just n -> C.linesUnbounded .| C.isolate (n-1) .| C.unlines .| C.fold

        hasMoreText <- C.peek
        C.leftover initialText

        case hasMoreText of
            Nothing -> unpagedOutput
            Just _ -> findPager >>= maybe fallback pagerConduit

findPager :: MonadIO m => m (Maybe CreateProcess)
findPager = runMaybeT $ asum
    [ Process.proc <$> find "less" <*> pure ["-RS", "-+X"]
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
        logThrowM $ unexpectedTermination process ec
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
