{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.WrappedPersistent
    ( Entity(..)
    , EntityField
    , Filter
    , Sqlite.PersistEntity
    , SqliteConnectionInfo
    , Unique
    , Update
    , SelectOpt(..)
    , toSqlKey
    , Sqlite.persistIdField
    , Sqlite.fieldLens
    , Sqlite.fromPersistValue
    , (Sqlite.=.)
    , (Sqlite.==.)
    , (Sqlite.+=.)
    , (Sqlite.||.)
    , module Sql.Core
    , module Sql.WrappedPersistent
    ) where

import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Data.Conduit (ConduitT, toProducer)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite
    ( AtLeastOneUniqueKey
    , Entity(..)
    , EntityField
    , Filter(..)
    , FilterValue(..)
    , OnlyOneUniqueKey
    , PersistFilter(..)
    , SelectOpt(..)
    , SqliteConnectionInfo
    , Unique
    , Update
    , toSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite
import Lens.Micro.Extras (view)

import Sql.Core

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
