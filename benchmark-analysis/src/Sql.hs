{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql
    ( Entity(..)
    , EntityField
    , Filter
    , Key
    , RawSqlite
    , SqliteConnectionInfo
    , SqlBackend
    , Unique
    , Update
    , fromSqlKey
    , toSqlKey
    , Sqlite.fieldLens
    , (Sqlite.=.)
    , (Sqlite.==.)
    , (Sqlite.+=.)
    , module Sql
    ) where

import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, (.|), runConduitRes, toProducer)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
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
    , RawSqlite
    , SelectOpt
    , SqliteConnectionInfo
    , SqlBackend
    , ToBackendKey
    , Unique
    , Update
    , (==.)
    , liftPersist
    , fromSqlKey
    , toSqlKey
    )
import qualified Database.Persist.Sqlite as Sqlite
import Database.Persist.Types (PersistValue)

import Schema

type MonadSql m = (MonadIO m, MonadReader (RawSqlite SqlBackend) m)
type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

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

queryExternalImplementations
    :: (MonadSql m, MonadUnliftIO m)
    => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId = runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadSql m, MonadUnliftIO m)
    => Key Algorithm -> m (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

    mkImpl :: Text -> Text -> Implementation
    mkImpl short long = Implementation algoId short (Just long) Nothing Builtin

    builtinImpls :: IntMap Implementation
    builtinImpls = IM.fromList
        [ (predictedImplId, mkImpl "predicted" "Predicted")
        , (bestNonSwitchingImplId, mkImpl "best" "Best Non-switching")
        , (optimalImplId, mkImpl "optimal" "Optimal")
        ]

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

rawExecute :: MonadSql m => Text -> [PersistValue] -> m ()
rawExecute query args = liftPersist $ Sqlite.rawExecute query args

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
