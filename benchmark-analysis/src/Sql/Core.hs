{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( Avg(..)
    , Max(..)
    , MonadExplain(..)
    , MonadRegion(..)
    , MonadQuery
    , MonadSql(..)
    , Region
    , SqlField
    , SqlPool
    , SqlRecord
    , SqlT
    , runSqlT
    , Transaction(Transaction)
    , abortTransaction
    , runTransaction
    , runReadOnlyTransaction
    , runTransactionWithoutForeignKeys
    , tryAbortableTransaction
    , readOnlyConnection
    , runRegionSource
    , runRegionConduit
    , conduitQuery
    , executeSql
    , executeSqlSingleValueMaybe
    , executeSqlSingleValue
    , getJust
    , getMigration
    , querySingleValue
    , runMigrationQuiet
    , runMigrationUnsafeQuiet
    , runRegion
    , selectKeys
    , selectKeysRegion
    , selectSource
    , selectSourceRegion
    , setPragma
    , setPragmaConn
    , showSqlKey
    , sinkQuery

    -- Re-exports
    , AtLeastOneUniqueKey
    , Entity(..)
    , EntityField
    , Filter
    , Key
    , Migration
    , MonadLogger
    , MonadThrow
    , PersistEntity
    , PersistField
    , PersistFieldSql
    , PersistRecordBackend
    , RawSqlite
    , SelectOpt(..)
    , SqlBackend
    , SqliteConnectionInfo
    , ToBackendKey
    , Unique
    , Update
    , (=.)
    , (==.)
    , (+=.)
    , (||.)
    , fieldLens
    , fromPersistValue
    , fromSqlKey
    , persistIdField
    , toSqlKey
    ) where

import Control.Monad (join, void)
import Control.Monad.Catch (handle, throwM)
import Control.Monad.IO.Unlift (MonadIO(liftIO), MonadUnliftIO(..))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logErrorN)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT, withReaderT)
import Control.Monad.State.Strict (StateT, modify', runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Resource
    (MonadResource, ReleaseKey, ResourceT, release)
import Data.Acquire (Acquire, allocateAcquire, mkAcquire)
import Data.Conduit (ConduitT, Void, (.|), runConduit, toProducer, transPipe)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite
    ( AtLeastOneUniqueKey
    , BackendCompatible
    , Entity(..)
    , EntityField
    , Filter
    , Key
    , Migration
    , PersistEntity
    , PersistField
    , PersistFieldSql
    , PersistRecordBackend
    , PersistValue
    , RawSqlite
    , SelectOpt(..)
    , Single(Single)
    , SqlBackend
    , SqliteConnectionInfo
    , ToBackendKey
    , Unique
    , Update
    , (=.)
    , (==.)
    , (+=.)
    , (||.)
    , fieldLens
    , fromPersistValue
    , fromSqlKey
    , persistIdField
    , toSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite
import Database.Sqlite.Internal (Connection(..), Connection'(..))
import Foreign (Ptr)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens

import Exceptions
import SQLiteExts
import qualified Sql.Core.PersistCompat as Compat

newtype Avg = Avg { getAvg :: Int } deriving (Show, Eq, Ord)
newtype Max = Max { getMax :: Int } deriving (Show, Eq, Ord)

class MonadResource m => MonadSql m where
    getConnFromPool :: m (Acquire (RawSqlite SqlBackend))
    getConnWithoutForeignKeysFromPool :: m (Acquire (RawSqlite SqlBackend))
    getConnWithoutTransaction :: m (Acquire (RawSqlite SqlBackend))

instance MonadSql m => MonadSql (ConduitT a b m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool
    getConnWithoutTransaction = lift getConnWithoutTransaction

instance MonadSql m => MonadSql (ReaderT r m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool
    getConnWithoutTransaction = lift getConnWithoutTransaction

instance MonadSql m => MonadSql (ResourceT m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool
    getConnWithoutTransaction = lift getConnWithoutTransaction

class Monad m => MonadExplain m where
    shouldExplainQuery :: Text -> m Bool
    shouldLogQuery :: Text -> m Bool

instance MonadExplain m => MonadExplain (Region m) where
    shouldExplainQuery = lift . shouldExplainQuery
    shouldLogQuery = lift . shouldLogQuery

instance MonadExplain m => MonadExplain (SqlT m) where
    shouldExplainQuery = lift . shouldExplainQuery
    shouldLogQuery = lift . shouldLogQuery

instance MonadExplain m => MonadExplain (ConduitT a b m) where
    shouldExplainQuery = lift . shouldExplainQuery
    shouldLogQuery = lift . shouldLogQuery

type MonadQuery m =
    (MonadResource m, MonadSql m, MonadExplain m, MonadLogger m, MonadThrow m)

newtype Transaction m r = Transaction (ReaderT (RawSqlite SqlBackend) m r)
  deriving
  ( Functor, Applicative, Monad, MonadCatch, MonadFail, MonadIO, MonadLogger
  , MonadResource, MonadThrow, MonadTrans
  )

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

readOnlyConnection :: RawSqlite SqlBackend -> Acquire (RawSqlite SqlBackend)
readOnlyConnection conn = mkAcquire mkReadOnly mkReadWrite
  where
    mkReadOnly = conn <$ setPragmaConn "query_only" (1 :: Int) conn
    mkReadWrite _ = setPragmaConn "query_only" (0 :: Int) conn

abortTransaction :: MonadThrow m => Text -> Transaction m r
abortTransaction txt = Transaction . throwM $ AbortTransaction txt

runRawTransaction
    :: MonadSql m
    => m (Acquire (RawSqlite SqlBackend)) -> Transaction m r -> m r
runRawTransaction getConn (Transaction transaction) = do
    (key, conn) <- getConn >>= allocateAcquire
    runReaderT transaction conn <* release key

runTransaction :: MonadSql m => Transaction m r -> m r
runTransaction = runRawTransaction getConnFromPool

runReadOnlyTransaction :: MonadSql m => Transaction m r -> m r
runReadOnlyTransaction = runRawTransaction getReadOnlyConn
  where
    getReadOnlyConn = (>>= readOnlyConnection) <$> getConnFromPool

runTransactionWithoutForeignKeys :: MonadSql m => Transaction m r -> m r
runTransactionWithoutForeignKeys =
    runRawTransaction getConnWithoutForeignKeysFromPool

tryAbortableTransaction
    :: (MonadCatch m, MonadLogger m, MonadSql m) => Transaction m () -> m ()
tryAbortableTransaction = handle abortException . runTransaction
  where
    abortException :: MonadLogger m => AbortTransaction -> m ()
    abortException (AbortTransaction msg) =
      logErrorN $ "Transaction aborted: " <> msg

newtype Region m r = Region (StateT (IO ()) m r)
  deriving
  ( Functor, Applicative, Monad, MonadCatch, MonadFail, MonadMask, MonadIO
  , MonadLogger, MonadLoggerIO, MonadResource, MonadThrow, MonadTrans)

class Monad m => MonadRegion m where
    allocRegion :: Acquire a -> m (ReleaseKey, a)

    allocRegion_ :: Acquire a -> m a
    allocRegion_ alloc = snd <$> allocRegion alloc

instance MonadResource m => MonadRegion (Region m) where
    allocRegion alloc = Region $ do
        result@(key, _) <- allocateAcquire alloc
        result <$ modify' (release key >>)

instance MonadResource m => MonadRegion (ConduitT a b (Region m)) where
    allocRegion alloc = lift $ allocRegion alloc

instance MonadSql m => MonadSql (Region m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool
    getConnWithoutTransaction = lift getConnWithoutTransaction

runRegion :: MonadIO m => Region m r -> m r
runRegion (Region act) = do
    (result, cleanup) <- runStateT act (return ())
    result <$ liftIO cleanup

runRegionSource
    :: MonadIO m => ConduitT () o (Region m) () -> ConduitT o Void m r -> m r
runRegionSource src sink = runRegion . runConduit $ src .| transPipe lift sink

runRegionConduit :: MonadIO m => ConduitT () Void (Region m) r -> m r
runRegionConduit conduit = runRegion $ runConduit conduit

liftProjectPersist
    :: (BackendCompatible sup (RawSqlite SqlBackend), MonadSql m)
    => ReaderT sup IO a -> Transaction m a
liftProjectPersist =
    Transaction . Sqlite.liftPersist . withReaderT Sqlite.projectBackend

setPragma :: (MonadSql m, Show v) => Text -> v -> Transaction m ()
setPragma pragma val = Transaction $ Sqlite.rawExecute query []
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

setPragmaConn
    :: (MonadIO m, Show v) => Text -> v -> RawSqlite SqlBackend -> m ()
setPragmaConn pragma val = runReaderT (Sqlite.rawExecute query [])
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

executeSql :: MonadSql m => Text -> Transaction m ()
executeSql query = Transaction $ Sqlite.rawExecute query []

executeSqlSingleValueMaybe
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> Transaction m (Maybe a)
executeSqlSingleValueMaybe query = Transaction $ do
    result <- Sqlite.rawSql query []
    case result of
        [] -> return Nothing
        [Single v] -> return $ Just v
        _ -> logThrowM $ ExpectedSingleValue query

executeSqlSingleValue
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> Transaction m a
executeSqlSingleValue query = do
    result <- executeSqlSingleValueMaybe query
    case result of
        Just v -> return v
        Nothing -> logThrowM QueryReturnedZeroResults

sinkQuery
    :: MonadResource m
    => Text
    -> [PersistValue]
    -> ConduitT [PersistValue] Void (Transaction m) r
    -> Transaction m r
sinkQuery query args sink = do
    (key, source) <- Transaction $ do
        acquireQuery <- Sqlite.rawQueryRes query args
        allocateAcquire acquireQuery

    runConduit (source .| sink) <* release key

selectKeys
    :: (MonadResource m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT (Key record) Void (Transaction m) r
    -> Transaction m r
selectKeys filts order sink = do
    (key, source) <- Transaction $ do
        acquireQuery <- Sqlite.selectKeysRes filts order
        allocateAcquire acquireQuery

    runConduit (source .| sink) <* release key

selectKeysRegion
    :: (MonadSql m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Key record) (Region m) ()
selectKeysRegion filts order = do
    acquireConn <- getConnFromPool
    (key, source) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.selectKeysRes filts order) conn

    toProducer source <* release key

selectSource
    :: (MonadResource m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT (Entity record) Void (Transaction m) r
    -> Transaction m r
selectSource filts order sink = do
    (key, source) <- Transaction $ do
        acquireQuery <- Sqlite.selectSourceRes filts order
        allocateAcquire acquireQuery

    runConduit (source .| sink) <* release key

selectSourceRegion
    :: (MonadSql m, SqlRecord record)
    => [Filter record]
    -> [SelectOpt record]
    -> ConduitT a (Entity record) (Region m) ()
selectSourceRegion filts order = do
    acquireConn <- getConnFromPool
    (key, source) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.selectSourceRes filts order) conn

    toProducer source <* release key

conduitQuery
    :: MonadSql m
    => Text
    -> [PersistValue]
    -> ConduitT a [PersistValue] (Region m) ()
conduitQuery query args = do
    acquireConn <- getConnFromPool
    (key, querySource) <- allocRegion $ do
        conn <- acquireConn >>= readOnlyConnection
        join $ runReaderT (Sqlite.rawQueryRes query args) conn

    toProducer querySource <* release key

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> m rec
getJust = runReadOnlyTransaction . Transaction . Sqlite.getJust

querySingleValue
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> [PersistValue] -> m a
querySingleValue query args = runTransaction . Transaction $ do
    result <- Sqlite.rawSql query args
    case result of
        [] -> logThrowM QueryReturnedZeroResults
        [Single v] -> return v
        _ -> logThrowM $ ExpectedSingleValue query

getMigration :: (MonadSql m) => Migration -> Transaction m [Text]
getMigration = liftProjectPersist . Sqlite.getMigration

runMigrationQuiet :: (MonadSql m) => Migration -> Transaction m [Text]
runMigrationQuiet = liftProjectPersist . Sqlite.runMigrationQuiet

runMigrationUnsafeQuiet :: (MonadSql m) => Migration -> Transaction m ()
runMigrationUnsafeQuiet =
    void . liftProjectPersist . Sqlite.runMigrationUnsafeQuiet

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = T.pack . show . Sqlite.fromSqlKey

type SqlPool = Pool (RawSqlite SqlBackend)

newtype SqlT m a = SqlT { unSqlT :: ReaderT SqlPool m a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger
  , MonadLoggerIO, MonadMask, MonadResource, MonadThrow)

instance MonadResource m => MonadSql (SqlT m) where
    getConnFromPool = SqlT $ asks Compat.acquireSqlConnFromPool
    getConnWithoutForeignKeysFromPool = do
        pool <- SqlT $ ask
        return $ do
            conn <- Compat.unsafeAcquireSqlConnFromPool pool
            let foreignOff = setPragmaConn "foreign_keys" (0 :: Int64)
                foreignOn _ = setPragmaConn "foreign_keys" (1 :: Int64) conn
            () <- mkAcquire (foreignOff conn) foreignOn
            Sqlite.acquireSqlConn conn

    getConnWithoutTransaction = SqlT $ asks Compat.unsafeAcquireSqlConnFromPool

instance (MonadLogger m, MonadThrow m) => MonadFail (SqlT m) where
    fail = logThrowM . PatternFailed . T.pack

instance MonadTrans SqlT where
    lift = SqlT . lift

instance MonadUnliftIO m => MonadUnliftIO (SqlT m) where
    withRunInIO inner = SqlT $ withRunInIO $ \run -> inner (run . unSqlT)
    {-# INLINE withRunInIO #-}

runSqlT
    :: (MonadLogger m, MonadThrow m, MonadUnliftIO m)
    => Text -> SqlT m r -> m r
runSqlT database (SqlT sqlAct) = runPool $ runReaderT sqlAct
  where
    runPool
        :: (MonadLogger m, MonadUnliftIO m, MonadThrow m)
        => (SqlPool -> m r) -> m r
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
