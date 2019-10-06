{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , showSqlKey
    , MonadIO(liftIO)
    , lift
    , MonadLogger
    , MonadLoggerIO
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
import Control.Monad.Logger
    (LoggingT, LogLevel(..), LogSource, MonadLogger, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Database.Persist.Sqlite as Sqlite
import Database.Sqlite.Internal (Connection(..), Connection'(..))
import Data.Conduit (ConduitT, (.|), awaitForever)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign (Ptr)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.ANSI (hGetTerminalSize)
import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))
import System.IO (Handle, IOMode(WriteMode))
import qualified System.IO as System

import Exceptions
import Migration
import Pretty (AnsiStyle, Doc, LayoutOptions(..), PageWidth(..))
import qualified Pretty
import Schema
import Sql.Core
import SQLiteExts

data Config = Config
    { explainHandle :: Maybe Handle
    , failTag :: Maybe Text
    , sqliteHandle :: RawSqlite SqlBackend
    }

data QueryMode = Normal | Explain | ExplainLog FilePath
    deriving (Eq, Read, Show)

newtype SqlM a = SqlM (ReaderT Config (ResourceT (LoggingT IO)) a)
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger
  , MonadLoggerIO, MonadMask, MonadResource, MonadThrow)

instance MonadException SqlM where
    controlIO f = join (withUnliftIO (f . unliftToRunIO))
      where
        unliftToRunIO :: Monad m => UnliftIO m -> RunIO m
        unliftToRunIO (UnliftIO g) = RunIO (fmap return . g)

instance MonadFail SqlM where
    fail s = SqlM $ do
        asks failTag >>= \case
            Nothing -> logThrowM . PatternFailed . Left $ T.pack s
            Just msg -> logThrowM . PatternFailed $ Right msg

instance MonadReader (RawSqlite SqlBackend) SqlM where
    ask = SqlM $ asks sqliteHandle
    local f (SqlM act) = SqlM $ local apply act
      where
        apply cfg = cfg { sqliteHandle = f (sqliteHandle cfg) }

instance MonadUnliftIO SqlM where
  askUnliftIO = SqlM $ withUnliftIO $ \u ->
                            return (UnliftIO (\(SqlM m) -> unliftIO u m))

class MonadFail m => MonadTagFail m where
    logIfFail :: Show v => Text -> v -> m a -> m a

instance MonadTagFail SqlM where
    logIfFail tag val = setTag $ tag <> ": " <> showText val
      where
        setTag :: Text -> SqlM a -> SqlM a
        setTag t (SqlM m) = SqlM $ local (\cfg -> cfg { failTag = Just t }) m

showText :: Show a => a -> Text
showText = T.pack . show

mIf :: Monoid m => Bool -> m -> m
mIf condition v
    | condition = v
    | otherwise = mempty

withTime :: MonadIO m => m a -> m (Double, a)
withTime act = do
    start <- liftIO $ getTime Monotonic
    r <- act
    end <- liftIO $ getTime Monotonic
    let wallTime :: Integer
        wallTime = toNanoSecs (diffTimeSpec start end)
    return $ (fromIntegral wallTime / 1e9, r)

data Options a =
  Options
    { database :: Text
    , vacuumDb :: Bool
    , logVerbosity :: Int
    , queryMode :: QueryMode
    , migrateSchema :: Bool
    , task :: a
    }

class Monad m => MonadExplain m where
    logQueryExplanation :: (Handle -> SqlM ()) -> m ()

instance MonadExplain SqlM where
    logQueryExplanation queryLogger = do
        queryHandle <- SqlM $ asks explainHandle
        case queryHandle of
            Nothing -> return ()
            Just hnd -> queryLogger hnd

instance MonadExplain m => MonadExplain (ConduitT a b m) where
    logQueryExplanation = lift . logQueryExplanation

stderrTerminalWidth :: IO (Maybe Int)
stderrTerminalWidth = runMaybeT $ do
    guard =<< liftIO (System.hIsTerminalDevice System.stderr)
    snd <$> MaybeT (hGetTerminalSize System.stderr)

topLevelHandler :: Bool -> SomeException -> IO ()
topLevelHandler quiet exc
    | Just (BenchmarkException e) <- fromException exc
    = when (not quiet) $ do
        pageWidth <- terminalToPageWidth <$> stderrTerminalWidth
        renderError pageWidth $ pretty e

    | otherwise = do
        pageWidth <- terminalToPageWidth <$> stderrTerminalWidth
        renderError pageWidth $ Pretty.vsep
            [ Pretty.reflow "Encountered an unexpected exception!"
            , "", pretty . T.pack $ displayException exc, ""
            , Pretty.reflow "Please file a bug report.\n"
            ]
  where
    terminalToPageWidth :: Maybe Int -> PageWidth
    terminalToPageWidth = maybe Unbounded (\n -> AvailablePerLine n 1)

    renderError :: PageWidth -> Doc AnsiStyle -> IO ()
    renderError p =
        Pretty.renderIO System.stderr . Pretty.layoutPretty (LayoutOptions p)

runSqlMWithOptions :: Options a -> (a -> SqlM b) -> IO b
runSqlMWithOptions Options{..} work = do
    setUncaughtExceptionHandler $ topLevelHandler (logVerbosity > 0)
    getNumProcessors >>= setNumCapabilities
    withQueryLog $ \mHnd -> runStack mHnd . wrapSqliteExceptions $ do
        sqlitePtr <- asks $ getSqlitePtr . Lens.view Sqlite.rawSqliteConnection

        registerSqlFunctions sqlitePtr

        didMigrate <- checkMigration migrateSchema

        -- Wait longer before timing out query steps
        setPragma "busy_timeout" (1000 :: Int64)

        -- Compacts and reindexes the database when request
        when (vacuumDb || didMigrate) $ executeSql "VACUUM"

        workResult <- work task

        -- Runs the ANALYZE command and updates query planner
        executeSql "PRAGMA optimize"

        return workResult
  where
    runStack
        :: Maybe Handle -> SqlM a -> IO a
    runStack mHnd act =
      runLog . Sqlite.withRawSqliteConnInfo connInfo $ runSql act . config
      where
        config = Config mHnd Nothing

    withQueryLog :: (Maybe Handle -> IO r) -> IO r
    withQueryLog f = case queryMode of
        Normal -> f Nothing
        Explain -> f (Just System.stdout)
        ExplainLog p -> System.withFile p WriteMode $ f . Just

    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logFilter

    runSql :: SqlM a -> Config -> LoggingT IO a
    runSql (SqlM act) cfg = runResourceT $ runReaderT act cfg

    connInfo :: SqliteConnectionInfo
    connInfo =
        Lens.set Sqlite.fkEnabled True $ Sqlite.mkSqliteConnectionInfo database

    getSqlitePtr :: Connection -> Ptr ()
    getSqlitePtr (Connection _ (Connection' ptr)) = ptr

    logFilter :: LogSource -> LogLevel -> Bool
    logFilter
        | logVerbosity <= 0 = \_ _ -> False
        | otherwise = \_ lvl -> lvl >= verbosity
      where
        verbosity = levels !! logVerbosity
        levels = LevelError : LevelWarn : LevelInfo : repeat LevelDebug

(.>) :: Monad m
     => ConduitT a b m ()
     -> (b -> ConduitT b c m r)
     -> ConduitT a c m ()
producer .> consumer = producer .| awaitForever consumer
infixl 3 .>
