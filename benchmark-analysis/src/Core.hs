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
    , WorkInput(..)
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
    , terminalWidth
    , withTime
    ) where

import Control.Monad (guard, when)
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LogLevel(..), LogSource, MonadLoggerIO)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Database.Persist.Sqlite as Sqlite
import Data.Acquire (mkAcquire, withAcquire)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, (.|), awaitForever)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.Terminal.Size (hSize, width)
import System.IO (Handle)
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

type SqlM = SqlT CoreM

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

data WorkInput a
    = ManualMigration MigrationSafety Int64
    | BuiltInCommand (SqlM ())
    | WorkInput a

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
    , workInput :: WorkInput a
    }

pagerSetting :: SqlM Pager
pagerSetting = lift . CoreM $ asks pagerValue

redirectLogging :: (ByteString -> IO ()) -> SqlM a -> SqlM a
redirectLogging logFun act = do
    ref <- lift $ asks logWriteRef
    let swapLogFun = atomicModifyIORef' ref (\oldFun -> (logFun, oldFun))
    withAcquire (mkAcquire swapLogFun (writeIORef ref)) $ \_ -> act

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

terminalWidth :: Handle -> IO (Maybe Int)
terminalWidth hnd = runMaybeT $ do
    guard =<< liftIO (System.hIsTerminalDevice hnd)
    width <$> MaybeT (hSize hnd)

topLevelHandler :: SomeException -> IO ()
topLevelHandler exc
    | Just (BenchmarkException True _) <- fromException exc
    = return ()

    | otherwise = do
        pageWidth <- terminalToPageWidth <$> terminalWidth System.stderr
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

runSqlMWithOptions :: forall a . Options a -> (a -> SqlM ()) -> IO ()
runSqlMWithOptions Options{..} work = do
    setUncaughtExceptionHandler topLevelHandler
    getNumProcessors >>= setNumCapabilities
    runSqlM . wrapSqliteExceptions $ runWork workInput

    when bellWhenDone (System.hPutStr System.stderr "\a")
  where
    runWork :: WorkInput a -> SqlM ()
    runWork (ManualMigration safety version) = migrateTo safety version
    runWork (WorkInput input) = withMigration $ work input
    runWork (BuiltInCommand act) = withMigration act

    withMigration :: SqlM () -> SqlM ()
    withMigration act = do
        didMigrate <- checkMigration migrateSchema

        -- Compacts and reindexes the database when request
        when (vacuumDb || didMigrate) $ do
            conn <- getConnWithoutTransaction
            withAcquire conn $ runReaderT (Sqlite.rawExecute "VACUUM" [])

        act

        -- Runs the ANALYZE command and updates query planner
        runTransaction $ executeSql "PRAGMA optimize"

    runSqlM :: SqlM () -> IO ()
    runSqlM sqlAct = do
        ref <- newIORef $ BS.hPutStr System.stderr
        runStack ref $ runSqlT database sqlAct
      where
        runStack :: IORef (ByteString -> IO ()) -> CoreM r -> IO r
        runStack ref (CoreM act) = runResourceT . runReaderT act $ config ref

        config :: IORef (ByteString -> IO ()) -> Config
        config ref = Config explainSet logSet pager ref logFilter

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
