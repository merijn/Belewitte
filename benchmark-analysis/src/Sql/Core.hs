{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( Entity(..)
    , EntityField
    , Filter
    , Key
    , RawSqlite
    , SqliteConnectionInfo
    , SqlBackend
    , Unique
    , Update
    , SelectOpt(..)
    , Migration
    , fromSqlKey
    , toSqlKey
    , Sqlite.fieldLens
    , Sqlite.fromPersistValue
    , (Sqlite.=.)
    , (Sqlite.==.)
    , (Sqlite.+=.)
    , module Sql.Core
    ) where

import Control.Monad.IO.Unlift (MonadIO)
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (MonadReader, ReaderT, withReaderT)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import Data.Acquire (Acquire)
import Data.Conduit (ConduitT, toProducer)
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
    , liftPersist
    , fromSqlKey
    , toSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite

import Exceptions

type MonadSql m = (MonadIO m, MonadReader (RawSqlite SqlBackend) m)
type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

liftProjectPersist
    :: (BackendCompatible sup (RawSqlite SqlBackend), MonadSql m)
    => ReaderT sup IO a
    -> m a
liftProjectPersist = liftPersist . withReaderT Sqlite.projectBackend

setPragma :: (MonadSql m, Show v) => Text -> v -> m ()
setPragma pragma val = liftPersist $ Sqlite.rawExecute query []
  where
    query = "PRAGMA " <> pragma <> " = " <> T.pack (show val)

executeSql :: MonadSql m => Text -> m ()
executeSql query = liftPersist $ Sqlite.rawExecute query []

conduitQuery
    :: (MonadResource m, MonadSql m)
    => Text -> [PersistValue] -> ConduitT () [PersistValue] m ()
conduitQuery query args = Sqlite.rawQuery query args

conduitQueryRes
    :: (MonadIO m, MonadSql n)
    => Text -> [PersistValue] -> n (Acquire (ConduitT () [PersistValue] m ()))
conduitQueryRes query args = liftPersist $ Sqlite.rawQueryRes query args

querySingleValue
    :: (MonadSql m, MonadLogger m, MonadThrow m, PersistField a)
    => Text
    -> [PersistValue]
    -> m a
querySingleValue query args = do
    result <- liftPersist $ Sqlite.rawSql query args
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

-- Generalisations
deleteWhere :: (MonadSql m, SqlRecord rec) => [Filter rec] -> m ()
deleteWhere = liftPersist . Sqlite.deleteWhere

getBy :: (MonadSql m, SqlRecord rec) => Unique rec -> m (Maybe (Entity rec))
getBy = liftPersist . Sqlite.getBy

getEntity :: (MonadSql m, SqlRecord rec) => Key rec -> m (Maybe (Entity rec))
getEntity = liftPersist . Sqlite.getEntity

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> m rec
getJust = liftPersist . Sqlite.getJust

insert :: (MonadSql m, SqlRecord rec) => rec -> m (Key rec)
insert = liftPersist . Sqlite.insert

insert_ :: (MonadSql m, SqlRecord rec) => rec -> m ()
insert_ = liftPersist . Sqlite.insert_

insertBy
    :: (AtLeastOneUniqueKey rec,  MonadSql m, SqlRecord rec)
    => rec -> m (Either (Entity rec) (Key rec))
insertBy = liftPersist . Sqlite.insertBy

onlyUnique
    :: (MonadSql m, OnlyOneUniqueKey rec, SqlRecord rec)
    => rec -> m (Unique rec)
onlyUnique = liftPersist . Sqlite.onlyUnique

getMigration :: MonadSql m => Migration -> m [Text]
getMigration = liftProjectPersist . Sqlite.getMigration

runMigrationSilent :: MonadSql m => Migration -> m [Text]
runMigrationSilent = liftProjectPersist . Sqlite.runMigrationSilent

runMigrationUnsafe :: MonadSql m => Migration -> m ()
runMigrationUnsafe = liftProjectPersist . Sqlite.runMigrationUnsafe

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m (Maybe (Entity rec))
selectFirst filters select = liftPersist $ Sqlite.selectFirst filters select

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Key rec]
selectKeysList filters select =
  liftPersist $ Sqlite.selectKeysList filters select

selectKeys
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> ConduitT a (Key rec) m ()
selectKeys filters select = toProducer $ Sqlite.selectKeys filters select

selectList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Entity rec]
selectList filters select = liftPersist $ Sqlite.selectList filters select

selectSource
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> ConduitT a (Entity rec) m ()
selectSource filters select = toProducer $ Sqlite.selectSource filters select

count :: (MonadSql m, SqlRecord rec) => [Filter rec] -> m Int
count = liftPersist . Sqlite.count

update :: (MonadSql m, SqlRecord rec) => Key rec -> [Update rec] -> m ()
update key = liftPersist . Sqlite.update key

updateWhere
    :: (MonadSql m, SqlRecord rec) => [Filter rec] -> [Update rec] -> m ()
updateWhere filts  = liftPersist . Sqlite.updateWhere filts

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = T.pack . show . Sqlite.fromSqlKey

likeFilter :: EntityField record Text -> Text -> Filter record
likeFilter field val = Filter field filterVal backendFilter
  where
    filterVal = FilterValue $ T.concat ["%", val, "%"]
    backendFilter = BackendSpecificFilter "like"

whenNotExists
    :: (MonadSql m, SqlRecord record)
    => [Filter record] -> ConduitT i a m () -> ConduitT i a m ()
whenNotExists filters act = selectFirst filters [] >>= \case
    Just _ -> return ()
    Nothing -> act
