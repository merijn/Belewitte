{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( Entity(..)
    , EntityField
    , Filter
    , Key
    , Sqlite.PersistEntity
    , Sqlite.PersistField
    , Sqlite.PersistFieldSql
    , RawSqlite
    , SqliteConnectionInfo
    , SqlBackend
    , Unique
    , Update
    , SelectOpt(..)
    , ToBackendKey
    , Migration
    , fromSqlKey
    , toSqlKey
    , Sqlite.persistIdField
    , Sqlite.fieldLens
    , Sqlite.fromPersistValue
    , (Sqlite.=.)
    , (Sqlite.==.)
    , (Sqlite.+=.)
    , (Sqlite.||.)
    , module Sql.Core
    ) where

import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (ReaderT, asks, runReaderT, withReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow, ResourceT, release)
import Data.Acquire (Acquire, allocateAcquire)
import Data.Conduit (ConduitT, toProducer, transPipe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (Single(Single))
import Database.Persist.Sqlite
    ( AtLeastOneUniqueKey
    , Entity(..)
    , EntityField
    , Filter(..)
    , FilterValue(..)
    , Key
    , OnlyOneUniqueKey
    , PersistField
    , PersistFilter(..)
    , PersistRecordBackend
    , PersistValue
    , RawSqlite
    , SelectOpt(..)
    , SqliteConnectionInfo
    , SqlBackend
    , ToBackendKey
    , Unique
    , Update
    , Migration
    , BackendCompatible
    , fromSqlKey
    , toSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite
import Lens.Micro.Extras (view)

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

getUniq
    :: (MonadSql m, SqlRecord record, OnlyOneUniqueKey record)
    => record -> m (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq
    :: (MonadLogger m, MonadSql m, SqlRecord record, AtLeastOneUniqueKey record, Eq record, Show record)
    => record -> m ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> Log.logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

fieldFromEntity
    :: Sqlite.PersistEntity r => EntityField r v -> Entity r -> v
fieldFromEntity field = view (Sqlite.fieldLens field)

-- Generalisations
deleteBy :: (MonadSql m, SqlRecord rec) => Unique rec -> m ()
deleteBy = runSql . Sqlite.deleteBy

deleteWhere
    :: (MonadSql m, SqlRecord rec) => [Filter rec] -> m ()
deleteWhere = runSql . Sqlite.deleteWhere

get :: (MonadSql m, SqlRecord rec) => Key rec -> m (Maybe rec)
get = runSql . Sqlite.get

getBy
    :: (MonadSql m, SqlRecord rec)
    => Unique rec -> m (Maybe (Entity rec))
getBy = runSql . Sqlite.getBy

getEntity
    :: (MonadSql m, SqlRecord rec)
    => Key rec -> m (Maybe (Entity rec))
getEntity = runSql . Sqlite.getEntity

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> m rec
getJust = runSql . Sqlite.getJust

getJustEntity
    :: (MonadSql m, SqlRecord rec) => Key rec -> m (Entity rec)
getJustEntity = runSql . Sqlite.getJustEntity

insert :: (MonadSql m, SqlRecord rec) => rec -> m (Key rec)
insert = runSql . Sqlite.insert

insert_ :: (MonadSql m, SqlRecord rec) => rec -> m ()
insert_ = runSql . Sqlite.insert_

insertBy
    :: (AtLeastOneUniqueKey rec, MonadSql m, SqlRecord rec)
    => rec -> m (Either (Entity rec) (Key rec))
insertBy = runSql . Sqlite.insertBy

onlyUnique
    :: (MonadSql m, OnlyOneUniqueKey rec, SqlRecord rec)
    => rec -> m (Unique rec)
onlyUnique = runSql . Sqlite.onlyUnique

getMigration :: (MonadSql m) => Migration -> m [Text]
getMigration = liftProjectPersist . Sqlite.getMigration

runMigrationQuiet :: (MonadSql m) => Migration -> m [Text]
runMigrationQuiet = liftProjectPersist . Sqlite.runMigrationQuiet

runMigrationUnsafeQuiet :: (MonadSql m) => Migration -> m ()
runMigrationUnsafeQuiet =
    void . liftProjectPersist . Sqlite.runMigrationUnsafeQuiet

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m (Maybe (Entity rec))
selectFirst filters select = runSql $ Sqlite.selectFirst filters select

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Key rec]
selectKeysList filters select = runSql $ Sqlite.selectKeysList filters select

selectKeys
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> ConduitT a (Key rec) m ()
selectKeys filters select =
    toProducer . liftPersistConduit $ Sqlite.selectKeys filters select

selectList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Entity rec]
selectList filters select = runSql $ Sqlite.selectList filters select

selectSource
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> ConduitT a (Entity rec) m ()
selectSource filters select =
  toProducer . liftPersistConduit $ Sqlite.selectSource filters select

count :: (MonadSql m, SqlRecord rec) => [Filter rec] -> m Int
count = runSql . Sqlite.count

update
    :: (MonadSql m, SqlRecord rec)
    => Key rec -> [Update rec] -> m ()
update key = runSql . Sqlite.update key

updateWhere
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [Update rec] -> m ()
updateWhere filts  = runSql . Sqlite.updateWhere filts

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = T.pack . show . Sqlite.fromSqlKey

likeFilter :: PersistField v => EntityField record v -> Text -> Filter record
likeFilter field val = Filter field filterVal backendFilter
  where
    filterVal = UnsafeValue $ T.concat ["%", val, "%"]
    backendFilter = BackendSpecificFilter "like"

whenNotExists
    :: (MonadSql m, SqlRecord record)
    => [Filter record] -> ConduitT i a m () -> ConduitT i a m ()
whenNotExists filters act = selectFirst filters [] >>= \case
    Just _ -> return ()
    Nothing -> act
