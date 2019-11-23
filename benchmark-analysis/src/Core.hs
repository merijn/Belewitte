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
    , Log.logDebugNS
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
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger
    (LoggingT, LogLevel(..), LogSource, MonadLogger, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Database.Persist.Sqlite as Sqlite
import Database.Sqlite.Internal (Connection(..), Connection'(..))
import Data.Acquire (mkAcquire, withAcquire)
import Data.Conduit (ConduitT, (.|), awaitForever)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Foreign (Ptr)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))
import System.Console.Terminal.Size (hSize, width)
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
    , pagerValue :: Pager
    , sqlitePool :: Pool (RawSqlite SqlBackend)
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

instance MonadSql SqlM where
    getConnFromPool = SqlM . asks $ Sqlite.acquireSqlConnFromPool . sqlitePool
    getConnWithoutForeignKeysFromPool = do
        pool <- SqlM $ asks sqlitePool
        return $ do
            conn <- Sqlite.unsafeAcquireSqlConnFromPool pool
            let foreignOff = setPragmaConn "foreign_keys" (0 :: Int64)
                foreignOn _ = setPragmaConn "foreign_keys" (1 :: Int64) conn
            () <- mkAcquire (foreignOff conn) foreignOn
            Sqlite.acquireSqlConn conn

instance MonadUnliftIO SqlM where
  askUnliftIO = SqlM $ withUnliftIO $ \u ->
                            return (UnliftIO (\(SqlM m) -> unliftIO u m))

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

data Pager = Never | Auto | Always deriving (Show)

data Options a =
  Options
    { database :: Text
    , vacuumDb :: Bool
    , logLevel :: LogLevel
    , debugPrefixes :: Maybe (Set Text)
    , queryMode :: QueryMode
    , migrateSchema :: Bool
    , pager :: Pager
    , task :: a
    }

pagerSetting :: SqlM Pager
pagerSetting = SqlM $ asks pagerValue

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
    width <$> MaybeT (hSize System.stderr)

topLevelHandler :: SomeException -> IO ()
topLevelHandler exc
    | Just (BenchmarkException _) <- fromException exc
    = return ()

    | otherwise = do
        pageWidth <- terminalToPageWidth <$> stderrTerminalWidth
        renderError pageWidth $ Pretty.vsep
            [ Pretty.reflow "Encountered an unexpected exception!"
            , "", pretty . T.pack $ displayException exc, ""
            , Pretty.reflow "Please file a bug report.", ""
            ]
  where
    terminalToPageWidth :: Maybe Int -> PageWidth
    terminalToPageWidth = maybe Unbounded (\n -> AvailablePerLine n 1)

    renderError :: PageWidth -> Doc AnsiStyle -> IO ()
    renderError p =
        Pretty.renderIO System.stderr . Pretty.layoutPretty (LayoutOptions p)

runSqlMWithOptions :: Options a -> (a -> SqlM b) -> IO b
runSqlMWithOptions Options{..} work = do
    setUncaughtExceptionHandler topLevelHandler
    getNumProcessors >>= setNumCapabilities
    withQueryLog $ \mHnd -> runStack mHnd . wrapSqliteExceptions $ do

        didMigrate <- checkMigration migrateSchema

        -- Compacts and reindexes the database when request
        when (vacuumDb || didMigrate) $ do
            pool <- SqlM $ asks sqlitePool
            let conn = Sqlite.unsafeAcquireSqlConnFromPool pool
            withAcquire conn $ runReaderT (Sqlite.rawExecute "VACUUM" [])

        workResult <- work task

        -- Runs the ANALYZE command and updates query planner
        runTransaction $ executeSql "PRAGMA optimize"

        return workResult
  where
    runStack
        :: Maybe Handle -> SqlM a -> IO a
    runStack mHnd act =
      runLog . Sqlite.withRawSqlitePoolInfo connInfo setup 20 $ runSqlM act . config
      where
        config = Config mHnd pager
        setup conn = do
            registerSqlFunctions ptr
            -- Wait longer before timing out query steps
            setPragmaConn "busy_timeout" (1000 :: Int64) conn
          where
            ptr = getSqlitePtr $ Lens.view Sqlite.rawSqliteConnection conn

    withQueryLog :: (Maybe Handle -> IO r) -> IO r
    withQueryLog f = case queryMode of
        Normal -> f Nothing
        Explain -> f (Just System.stdout)
        ExplainLog p -> System.withFile p WriteMode $ f . Just

    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logFilter

    runSqlM :: SqlM a -> Config -> LoggingT IO a
    runSqlM (SqlM act) cfg = runResourceT $ runReaderT act cfg

    connInfo :: SqliteConnectionInfo
    connInfo =
        Lens.set Sqlite.fkEnabled True $ Sqlite.mkSqliteConnectionInfo database

    getSqlitePtr :: Connection -> Ptr ()
    getSqlitePtr (Connection _ (Connection' ptr)) = ptr

    logFilter :: LogSource -> LogLevel -> Bool
    logFilter src LevelDebug
        | srcInPrefixSet = True
        | otherwise = False
      where
        srcInPrefixSet = case debugPrefixes of
            Nothing -> True
            Just prefixes -> case S.lookupLE (T.toLower src) prefixes of
                Nothing -> False
                Just prefix -> T.isPrefixOf prefix (T.toLower src)

    logFilter _ lvl
        | lvl >= logLevel = True
        | otherwise = False

(.>) :: Monad m
     => ConduitT a b m ()
     -> (b -> ConduitT b c m r)
     -> ConduitT a c m ()
producer .> consumer = producer .| awaitForever consumer
infixl 3 .>
