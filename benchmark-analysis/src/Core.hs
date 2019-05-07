{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Core
    ( Int64
    , Key
    , Entity(..)
    , fromSqlKey
    , toSqlKey
    , MonadIO(liftIO)
    , MonadLogger
    , Log.logErrorN
    , Log.logWarnN
    , Log.logInfoN
    , Log.logDebugN
    , MonadResource
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    , withUnliftIO
    , UnliftIO(..)
    , ReaderT(..)
    , Text
    , module Core
    , module Exceptions
    ) where

import Control.Monad (guard, join, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, SomeException)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LoggingT, LogLevel(..), LogSource, MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), asks, local, mapReaderT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Database.Persist.Sqlite hiding (Connection)
import Database.Sqlite.Internal
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc, LayoutOptions(..), PageWidth(..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.Text.Prettyprint.Doc.Util as Pretty
import Foreign (Ptr)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.ANSI (getTerminalSize)
import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))
import System.IO (Handle, IOMode(WriteMode))
import qualified System.IO as System

import Exceptions
import Migration
import Schema
import SQLiteExts

type SqlM = ReaderT (RawSqlite SqlBackend) BaseM

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

data QueryMode = Normal | Explain | ExplainLog FilePath
    deriving (Eq, Read, Show)

newtype BaseM a = BaseM
  { runBaseM :: ReaderT (Maybe Text, Maybe Handle)
                        (ResourceT (LoggingT IO))
                        a
  }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger, MonadMask
  , MonadResource, MonadThrow)

instance MonadException BaseM where
    controlIO f = join (withUnliftIO (f . unliftToRunIO))
      where
        unliftToRunIO :: Monad m => UnliftIO m -> RunIO m
        unliftToRunIO (UnliftIO g) = RunIO (fmap return . g)

showText :: Show a => a -> Text
showText = T.pack . show

logIfFail :: Show v => Text -> v -> SqlM a -> SqlM a
logIfFail tag val = mapReaderT . setTag $ tag <> ": " <> showText val
  where
    setTag :: Text -> BaseM a -> BaseM a
    setTag t = BaseM . local (\(_, v) -> (Just t, v)) . runBaseM

withTime :: MonadIO m => m a -> m (Double, a)
withTime act = do
    start <- liftIO $ getTime Monotonic
    r <- act
    end <- liftIO $ getTime Monotonic
    let wallTime :: Integer
        wallTime = toNanoSecs (diffTimeSpec start end)
    return $ (fromIntegral wallTime / 1e9, r)

instance MonadFail BaseM where
    fail s = BaseM $ do
        asks fst >>= \case
            Nothing -> logThrowM . PatternFailed . Left $ T.pack s
            Just msg -> logThrowM . PatternFailed $ Right msg

instance MonadUnliftIO BaseM where
  askUnliftIO = BaseM $ withUnliftIO $ \u ->
                            return (UnliftIO (unliftIO u . runBaseM))

data Options a =
  Options
    { database :: Text
    , vacuumDb :: Bool
    , logVerbosity :: Int
    , queryMode :: QueryMode
    , migrateSchema :: Bool
    , task :: a
    }

logQueryExplanation :: (Handle -> SqlM ()) -> SqlM ()
logQueryExplanation queryLogger = do
    queryHandle <- lift . BaseM $ asks snd
    case queryHandle of
        Nothing -> return ()
        Just hnd -> queryLogger hnd

topLevelHandler :: Bool -> SomeException -> IO ()
topLevelHandler quiet exc
    | Just (BenchmarkException e) <- fromException exc
    = when (not quiet) $ do
        pageWidth <- getPageWidth
        renderError pageWidth $ pretty e

    | otherwise = do
        pageWidth <- getPageWidth
        renderError pageWidth $ Pretty.vsep
            [ Pretty.reflow "Encountered an unexpected exception!"
            , "", pretty . T.pack $ displayException exc, ""
            , Pretty.reflow "Please file a bug report."
            ]
  where
    getPageWidth :: IO PageWidth
    getPageWidth = fmap (fromMaybe Unbounded) . runMaybeT $ do
        isTerminal <- liftIO $ System.hIsTerminalDevice System.stderr
        guard isTerminal
        (_, n) <- MaybeT getTerminalSize
        return $ AvailablePerLine n 1

    renderError :: PageWidth -> Doc AnsiStyle -> IO ()
    renderError p =
        Pretty.renderIO System.stderr . Pretty.layoutPretty (LayoutOptions p)

runSqlMWithOptions :: Options a -> (a -> SqlM b) -> IO b
runSqlMWithOptions Options{..} work = do
    setUncaughtExceptionHandler $ topLevelHandler (logVerbosity > 0)
    mapException PrettySqliteException $ do
        getNumProcessors >>= setNumCapabilities
        withQueryLog $ \mHnd -> runStack (Nothing, mHnd) $ do
            sqlitePtr <- asks $ getSqlitePtr . Lens.view rawSqliteConnection

            registerSqlFunctions sqlitePtr

            didMigrate <- checkMigration migrateSchema

            -- Wait longer before timing out query steps
            rawExecute "PRAGMA busy_timeout = 1000" []

            -- Compacts and reindexes the database when request
            when (vacuumDb || didMigrate) $ rawExecute "VACUUM" []

            workResult <- work task

            -- Runs the ANALYZE command and updates query planner
            rawExecute "PRAGMA optimize" []

            return workResult
  where
    runStack
        :: (Maybe Text, Maybe Handle) -> SqlM a -> IO a
    runStack config =
      runLog . runBase config . withRawSqliteConnInfo connInfo . runReaderT

    withQueryLog :: (Maybe Handle -> IO r) -> IO r
    withQueryLog f = case queryMode of
        Normal -> f Nothing
        Explain -> f (Just System.stdout)
        ExplainLog p -> System.withFile p WriteMode $ f . Just

    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logFilter

    runBase :: (Maybe Text, Maybe Handle) -> BaseM a -> LoggingT IO a
    runBase cfg = runResourceT . (`runReaderT` cfg) . runBaseM

    connInfo :: SqliteConnectionInfo
    connInfo = Lens.set fkEnabled True $ mkSqliteConnectionInfo database

    getSqlitePtr :: Connection -> Ptr ()
    getSqlitePtr (Connection _ (Connection' ptr)) = ptr

    logFilter :: LogSource -> LogLevel -> Bool
    logFilter
        | logVerbosity <= 0 = \_ _ -> False
        | otherwise = \_ lvl -> lvl >= verbosity
      where
        verbosity = levels !! logVerbosity
        levels = LevelError : LevelWarn : LevelInfo : repeat LevelDebug

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = showText . fromSqlKey

likeFilter :: EntityField record Text -> Text -> Filter record
likeFilter field val =
  Filter field (Left $ T.concat ["%", val, "%"]) (BackendSpecificFilter "like")

whenNotExists
    :: SqlRecord record
    => [Filter record] -> ConduitT i a SqlM () -> ConduitT i a SqlM ()
whenNotExists filters act = lift (selectFirst filters []) >>= \case
    Just _ -> return ()
    Nothing -> act

getUniq :: SqlRecord record => record -> SqlM (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq
    :: (SqlRecord record, Eq record, Show record)
    => record -> SqlM ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> Log.logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

queryImplementations :: Key Algorithm -> SqlM (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

    mkImpl :: Text -> Text -> Implementation
    mkImpl short long =
        Implementation algoId short (Just long) Nothing Builtin False

    builtinImpls :: IntMap Implementation
    builtinImpls = IM.fromList
        [ (predictedImplId, mkImpl "predicted" "Predicted")
        , (bestNonSwitchingImplId, mkImpl "best" "Best Non-switching")
        , (optimalImplId, mkImpl "optimal" "Optimal")
        ]
