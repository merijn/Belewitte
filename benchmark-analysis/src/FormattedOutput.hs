{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module FormattedOutput
    ( renderEntity
    , printProxyEntity
    , renderColumns
    , renderColumnsSelect
    , renderOutput
    , renderRegionOutput
    ) where

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
import Data.Foldable (asum, fold)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.IO.Exception (IOException, IOErrorType(ResourceVanished))
import Lens.Micro.Extras (view)
import System.Console.Terminal.Size (hSize, height)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hIsTerminalDevice, stdout)
import System.IO.Error (ioeGetErrorType)

import Core
import Pretty.Fields
import Sql
import ProcessTools (unexpectedTermination)

fieldToText :: FieldInfo a -> a -> Text
fieldToText (VerboseFieldInfo accessor toText _ _) =
    case accessor of
        PersistField field -> toText . view (Sql.fieldLens field)
        RecordField getField _ -> toText . getField

fieldToVerboseText
    :: MonadSql m => FieldInfo a -> a -> m Text
fieldToVerboseText (VerboseFieldInfo accessor toText toTextM _) val =
    fromMaybe (return . toText) toTextM $ case accessor of
        PersistField field -> view (Sql.fieldLens field) val
        RecordField getField _ -> getField val

padText :: Int -> Text -> Text
padText n input = input <> T.replicate (n - T.length input) " "

queryFieldInfo
    :: (PrettyFields a, MonadQuery m)
    => (Text, FieldInfo a) -> m (Text, FieldInfo a, (Avg, Max))
queryFieldInfo (name, col@(VerboseFieldInfo _ _ _ True)) =
    return (name, col, (Avg 0, Max 0))

queryFieldInfo (name, col@(VerboseFieldInfo accessor _ _ False)) =
    annotateField <$> case accessor of
        PersistField field -> Sql.getFieldLength field
        RecordField _ getWidth -> getWidth
  where
    fieldSize = Max . T.length $ name
    annotateField (avgVal, maxVal)
        | maxVal > Max 0 = (name, col, (avgVal, max fieldSize maxVal))
        | otherwise = (name, col, (avgVal, maxVal))

renderEntity
    :: forall a m . (MonadSql m, PrettyFields a) => a -> m Text
renderEntity ent = fold <$> traverse formatLine fieldInfos
  where
    formatLine :: (Text, FieldInfo a) -> m Text
    formatLine (name, field) = do
        fieldTxt <- fieldToVerboseText field ent
        return $ mconcat [paddedName, "   ", fieldTxt , "\n"]
      where
        paddedName = padText (1 + maxLength) $ name <> ":"

    maxLength :: Int
    maxLength = maximum . fmap (T.length . fst) $ fieldInfos

    fieldInfos :: NonEmpty (Text, FieldInfo a)
    fieldInfos = prettyFieldInfo

printProxyEntity :: PrettyFields a => proxy a -> a -> SqlM ()
printProxyEntity _ ent = renderOutput $ renderEntity ent >>= C.yield

columnFormatter
    :: (MonadQuery m, PrettyFields a) => m (Text, a -> Text)
columnFormatter = do
    (startCol :| columns) <- traverse queryFieldInfo prettyFieldInfo
    let entityRenderer = padColumn startCol <> foldMap sepPadColumn columns
        headerText = header startCol <> foldMap sepHeader columns
    return (headerText, entityRenderer)
  where
    sep :: Text
    sep = "   "

    header :: PrettyFields a => (Text, FieldInfo a, (Avg, Max)) -> Text
    header (_, _, (_, Max 0)) = ""
    header (name, _, (_, Max n)) = padText n $ name

    sepHeader :: PrettyFields a => (Text, FieldInfo a, (Avg, Max)) -> Text
    sepHeader (_, _, (_, Max 0)) = ""
    sepHeader x = sep <> header x

    padColumn
        :: PrettyFields a
        => (Text, FieldInfo a, (Avg, Max)) -> a -> Text
    padColumn (_, _, (_, Max 0)) = const ""
    padColumn (_, field, (_, Max n)) = padText n . fieldToText field

    sepPadColumn
        :: PrettyFields a
        => (Text, FieldInfo a, (Avg, Max)) -> a -> Text
    sepPadColumn (_, _, (_, Max 0)) _ = ""
    sepPadColumn x y = sep <> padColumn x y

renderColumnsSelect
    :: (PrettyFields (Entity a), SqlRecord a)
    => [Filter a] -> [SelectOpt a] -> SqlM ()
renderColumnsSelect filts order =
  renderColumns $ Sql.selectSourceRegion filts order

renderColumns :: PrettyFields a => ConduitT () a (Region SqlM) () -> SqlM ()
renderColumns source = do
    (header, f) <- columnFormatter
    maybeHeight <- liftIO $ fmap height <$> hSize stdout

    let annotateHeaders = case maybeHeight of
            Nothing -> yield header >> C.map f
            Just n -> do
                yield header
                C.isolate (n-2) .| C.map f
                empty <- C.null
                unless empty $ annotateHeaders

    renderRegionOutput $ source .| annotateHeaders .| C.unlines

renderOutput :: ConduitT () Text SqlM () -> SqlM ()
renderOutput producer = do
    res <- try . runConduit $ producer .| outputSink
    case res of
        Right _ -> return ()
        Left (ioeGetErrorType -> ResourceVanished) -> return ()
        Left e -> throwM e

renderRegionOutput :: ConduitT () Text (Region SqlM) () -> SqlM ()
renderRegionOutput producer = do
    res <- try $ runRegionSource producer outputSink
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
    outputPager = C.linesUnbounded .| do
        maybeHeight <- liftIO $ fmap height <$> hSize stdout
        initialLines <- case maybeHeight of
            Nothing -> return []
            Just n -> C.isolate (n-1) .| C.foldl (flip (:)) []

        hasMoreText <- C.peek
        mapM_ C.leftover initialLines

        C.unlines .| case hasMoreText of
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
