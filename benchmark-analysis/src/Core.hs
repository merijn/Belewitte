{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    , module Exceptions
    , MonadExplain(..)
    , Options(..)
    , Pager(..)
    , SqlM
    , UTCTime
    , (.>)
    , logExplain
    , logQuery
    , mIf
    , pagerSetting
    , redirectLogging
    , runSqlMWithOptions
    , showText
    , stderrTerminalWidth
    , withTime
    ) where

import Control.Monad (guard, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, SomeException)
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger
    (LogLevel(..), LogSource, MonadLogger, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Database.Persist.Sqlite as Sqlite
import Database.Sqlite.Internal (Connection(..), Connection'(..))
import Data.Acquire (mkAcquire, withAcquire)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, (.|), awaitForever)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Foreign (Ptr)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.Terminal.Size (hSize, width)
import qualified System.IO as System
import System.Log.FastLogger (fromLogStr)

import Exceptions
import Migration
import Pretty (AnsiStyle, Doc, LayoutOptions(..), PageWidth(..))
import qualified Pretty
import Schema
import Sql.Core
import SQLiteExts

data Config = Config
    { explainQuerySet :: Maybe (Set Text)
    , logQuerySet :: Maybe (Set Text)
    , pagerValue :: Pager
    , logWriteRef :: IORef (ByteString -> IO ())
    , logFilterFun :: LogSource -> LogLevel -> Bool
    }

type SqlPool = Pool (RawSqlite SqlBackend)

newtype CoreM a = CoreM { unCoreM :: ReaderT Config (ResourceT IO) a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask
  , MonadReader Config, MonadResource, MonadThrow)

instance MonadLogger CoreM where
    monadLoggerLog loc src lvl msg = do
        logger <- Log.askLoggerIO
        liftIO $ logger loc src lvl (Log.toLogStr msg)

instance MonadLoggerIO CoreM where
    askLoggerIO = do
        logFilter <- asks logFilterFun
        ref <- asks logWriteRef
        return $ \loc src lvl msg -> when (logFilter src lvl) $ do
            logFun <- readIORef ref
            logFun . fromLogStr $ Log.defaultLogStr loc src lvl msg

instance MonadUnliftIO CoreM where
    withRunInIO inner = CoreM $ withRunInIO $ \run -> inner (run. unCoreM)
    {-# INLINE withRunInIO #-}

newtype SqlM a = SqlM { unSqlM :: ReaderT SqlPool CoreM a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger
  , MonadLoggerIO, MonadMask, MonadResource, MonadThrow)

instance MonadSql SqlM where
    getConnFromPool = SqlM $ asks Sqlite.acquireSqlConnFromPool
    getConnWithoutForeignKeysFromPool = do
        pool <- SqlM $ ask
        return $ do
            conn <- Sqlite.unsafeAcquireSqlConnFromPool pool
            let foreignOff = setPragmaConn "foreign_keys" (0 :: Int64)
                foreignOn _ = setPragmaConn "foreign_keys" (1 :: Int64) conn
            () <- mkAcquire (foreignOff conn) foreignOn
            Sqlite.acquireSqlConn conn

instance MonadUnliftIO SqlM where
    withRunInIO inner = SqlM $ withRunInIO $ \run -> inner (run. unSqlM)
    {-# INLINE withRunInIO #-}

logExplain :: MonadLogger m => Text -> Text -> m ()
logExplain name = Log.logOtherNS name (LevelOther "Explain")

logQuery :: MonadLogger m => Text -> Text -> m ()
logQuery name = Log.logOtherNS name (LevelOther "Query")

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
    , explainSet :: Maybe (Set Text)
    , logSet :: Maybe (Set Text)
    , migrateSchema :: Bool
    , pager :: Pager
    , bellWhenDone :: Bool
    , task :: a
    }

pagerSetting :: SqlM Pager
pagerSetting = SqlM . lift . CoreM $ asks pagerValue

redirectLogging :: (ByteString -> IO ()) -> SqlM a -> SqlM a
redirectLogging logFun (SqlM act) = SqlM $ do
    ref <- lift $ asks logWriteRef
    let swapLogFun = atomicModifyIORef' ref (\oldFun -> (logFun, oldFun))
    withAcquire (mkAcquire swapLogFun (writeIORef ref)) $ \_ -> act

class Monad m => MonadExplain m where
    shouldExplainQuery :: Text -> m Bool
    shouldLogQuery :: Text -> m Bool

instance MonadExplain CoreM where
    shouldExplainQuery name = do
        querySet <- asks explainQuerySet
        case querySet of
            Nothing -> return True
            Just names | S.member (T.toLower name) names -> return True
            _ -> return False

    shouldLogQuery name = do
        querySet <- asks logQuerySet
        case querySet of
            Nothing -> return True
            Just names | S.member (T.toLower name) names -> return True
            _ -> return False

instance MonadExplain m => MonadExplain (Region m) where
    shouldExplainQuery = lift . shouldExplainQuery
    shouldLogQuery = lift . shouldLogQuery

instance MonadExplain SqlM where
    shouldExplainQuery = SqlM . lift . shouldExplainQuery
    shouldLogQuery = SqlM . lift . shouldLogQuery

instance MonadExplain m => MonadExplain (ConduitT a b m) where
    shouldExplainQuery = lift . shouldExplainQuery
    shouldLogQuery = lift . shouldLogQuery

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
    initialiseSqlite
    setUncaughtExceptionHandler topLevelHandler
    getNumProcessors >>= setNumCapabilities
    result <- runSqlM . wrapSqliteExceptions $ do

        didMigrate <- checkMigration migrateSchema

        -- Compacts and reindexes the database when request
        when (vacuumDb || didMigrate) $ do
            pool <- SqlM ask
            let conn = Sqlite.unsafeAcquireSqlConnFromPool pool
            withAcquire conn $ runReaderT (Sqlite.rawExecute "VACUUM" [])

        workResult <- work task

        -- Runs the ANALYZE command and updates query planner
        runTransaction $ executeSql "PRAGMA optimize"

        return workResult

    result <$ when bellWhenDone (System.hPutStr System.stderr "\a")
  where
    runSqlM :: SqlM a -> IO a
    runSqlM sqlAct = do
        ref <- newIORef $ BS.hPutStr System.stderr
        runStack ref . runPool $ runReaderT (unSqlM sqlAct)
      where
        runStack :: IORef (ByteString -> IO ()) -> CoreM a -> IO a
        runStack ref (CoreM act) = runResourceT . runReaderT act $ config ref

        config :: IORef (ByteString -> IO ()) -> Config
        config ref = Config explainSet logSet pager ref logFilter

        runPool
            :: (MonadLogger m, MonadUnliftIO m, MonadThrow m)
            => (SqlPool -> m a) -> m a
        runPool = Sqlite.withRawSqlitePoolInfo connInfo setup 20

        setup
            :: (MonadIO m, MonadLogger m, MonadThrow m)
            => RawSqlite SqlBackend -> m ()
        setup conn = do
            registerSqlFunctions ptr
            -- Wait longer before timing out query steps
            setPragmaConn "threads" (4 :: Int64) conn
            setPragmaConn "busy_timeout" (5000 :: Int64) conn
          where
            ptr = getSqlitePtr $ Lens.view Sqlite.rawSqliteConnection conn

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
