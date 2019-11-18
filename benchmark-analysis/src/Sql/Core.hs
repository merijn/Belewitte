{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( Entity(..)
    , EntityField
    , Filter
    , Key
    , Migration
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
    , module Sql.Core
    ) where

import Control.Monad (join, void)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
    (MonadReader, ReaderT, asks, runReaderT, withReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, release)
import Data.Acquire (Acquire, allocateAcquire)
import Data.Conduit (ConduitT)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite
    ( BackendCompatible
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

import Exceptions

class MonadResource m => MonadSql m where
    getConnFromPool :: m (Acquire (RawSqlite SqlBackend))
    getConnWithoutForeignKeysFromPool :: m (Acquire (RawSqlite SqlBackend))

instance MonadSql m => MonadSql (ConduitT a b m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool

instance MonadSql m => MonadSql (ReaderT r m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool

instance MonadSql m => MonadSql (ResourceT m) where
    getConnFromPool = lift getConnFromPool
    getConnWithoutForeignKeysFromPool = lift getConnWithoutForeignKeysFromPool

newtype DummySql a =
  DummySql (ReaderT (Pool (RawSqlite SqlBackend)) (ResourceT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

instance MonadSql DummySql where
    getConnFromPool = DummySql $ asks Sqlite.acquireSqlConnFromPool
    getConnWithoutForeignKeysFromPool = getConnFromPool

newtype Transaction m r = Transaction
  { unTransactionT :: ReaderT (RawSqlite SqlBackend) m r }
  deriving
  ( Functor, Applicative, Monad, MonadCatch, MonadFail, MonadIO, MonadLogger
  , MonadReader (RawSqlite SqlBackend), MonadResource, MonadThrow
  , MonadTrans
  )

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

runTransaction :: MonadSql m => Transaction m r -> m r
runTransaction (Transaction transaction) = do
    (key, conn) <- getConnFromPool >>= allocateAcquire
    runReaderT transaction conn <* release key

runTransactionWithoutForeignKeys :: MonadSql m => Transaction m r -> m r
runTransactionWithoutForeignKeys (Transaction transaction) = do
    (key, conn) <- getConnWithoutForeignKeysFromPool >>= allocateAcquire
    runReaderT transaction conn <* release key

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

conduitQueryTrans
    :: MonadSql m
    => Text -> [PersistValue] -> ConduitT () [PersistValue] (Transaction m) ()
conduitQueryTrans query args = Sqlite.rawQuery query args

conduitQuery
    :: (MonadIO m, MonadSql n)
    => Text -> [PersistValue] -> n (Acquire (ConduitT () [PersistValue] m ()))
conduitQuery query args = do
    acquireConn <- getConnFromPool
    return $ do
        conn <- acquireConn
        join $ runReaderT (Sqlite.rawQueryRes query args) conn

querySingleValue
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> [PersistValue] -> m a
querySingleValue query args = runTransaction . Transaction $ do
    result <- Sqlite.rawSql query args
    case result of
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
