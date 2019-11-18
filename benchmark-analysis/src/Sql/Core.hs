{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( Key
    , PersistField
    , PersistFieldSql
    , PersistRecordBackend
    , RawSqlite
    , SqlBackend
    , ToBackendKey
    , fromSqlKey
    , module Sql.Core
    ) where

import Control.Monad (join, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT, asks, runReaderT, withReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, release)
import Data.Acquire (Acquire, allocateAcquire)
import Data.Conduit (ConduitT, transPipe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite
    ( Key
    , PersistField
    , PersistFieldSql
    , PersistRecordBackend
    , PersistValue
    , RawSqlite
    , Single(Single)
    , SqlBackend
    , ToBackendKey
    , Migration
    , BackendCompatible
    , fromSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite

import Exceptions

class MonadIO m => MonadSqlPool m where
    getConnFromPool :: m (Acquire (RawSqlite SqlBackend))

instance MonadSqlPool m => MonadSqlPool (ConduitT a b m) where
    getConnFromPool = lift getConnFromPool

instance MonadSqlPool m => MonadSqlPool (ReaderT r m) where
    getConnFromPool = lift getConnFromPool

instance MonadSqlPool m => MonadSqlPool (ResourceT m) where
    getConnFromPool = lift getConnFromPool

newtype DummySql a =
  DummySql (ReaderT (Pool (RawSqlite SqlBackend)) (ResourceT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

instance MonadSqlPool DummySql where
    getConnFromPool = DummySql $ asks Sqlite.acquireSqlConnFromPool

type MonadSql m = (MonadResource m, MonadSqlPool m)

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

runSql :: MonadSql m => ReaderT (RawSqlite SqlBackend) m r -> m r
runSql act = do
    (key, conn) <- getConnFromPool >>= allocateAcquire
    runReaderT act conn <* release key

liftPersistConduit
    :: MonadSql m
    => ConduitT a b (ReaderT (RawSqlite SqlBackend) m) r
    -> ConduitT a b m r
liftPersistConduit conduit = transPipe runSql conduit

liftProjectPersist
    :: (BackendCompatible sup (RawSqlite SqlBackend), MonadSql m)
    => ReaderT sup IO a -> m a
liftProjectPersist =
    runSql . Sqlite.liftPersist . withReaderT Sqlite.projectBackend

setPragma :: (MonadSql m, Show v) => Text -> v -> m ()
setPragma pragma val = runSql $ Sqlite.rawExecute query []
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

setPragmaConn
    :: (MonadIO m, Show v) => Text -> v -> RawSqlite SqlBackend -> m ()
setPragmaConn pragma val = runReaderT (Sqlite.rawExecute query [])
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

executeSql :: MonadSql m => Text -> m ()
executeSql query = runSql $ Sqlite.rawExecute query []

conduitQueryRes
    :: (MonadIO m, MonadSql n)
    => Text -> [PersistValue] -> n (Acquire (ConduitT () [PersistValue] m ()))
conduitQueryRes query args = do
    acquireConn <- getConnFromPool
    return $ do
        conn <- acquireConn
        join . liftIO $ runReaderT (Sqlite.rawQueryRes query args) conn

querySingleValue
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text
    -> [PersistValue]
    -> m a
querySingleValue query args = do
    result <- runSql $ Sqlite.rawSql query args
    case result of
        [Single v] -> return v
        _ -> logThrowM $ ExpectedSingleValue query

getMigration :: (MonadSql m) => Migration -> m [Text]
getMigration = liftProjectPersist . Sqlite.getMigration

runMigrationQuiet :: (MonadSql m) => Migration -> m [Text]
runMigrationQuiet = liftProjectPersist . Sqlite.runMigrationQuiet

runMigrationUnsafeQuiet :: (MonadSql m) => Migration -> m ()
runMigrationUnsafeQuiet =
    void . liftProjectPersist . Sqlite.runMigrationUnsafeQuiet
