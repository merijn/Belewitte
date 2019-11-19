{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Transaction (module Sql.Core, module Sql.Transaction) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, (.|), toProducer, runConduit)
import qualified Data.Conduit.Combinators as C
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sqlite
    ( AtLeastOneUniqueKey
    , Filter(..)
    , FilterValue(..)
    , OnlyOneUniqueKey
    , PersistFilter(..)
    , getFieldName
    , getTableName
    )
import qualified Database.Persist.Sqlite as Sqlite
import Lens.Micro.Extras (view)

import Exceptions
import Query (MonadQuery, Query(..), runSqlQuerySingle)
import Schema
import Sql.Core

validateEntity
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , SqlRecord r
       , ToBackendKey SqlBackend r
       )
    => Text -> Int64 -> Transaction m (Entity r)
validateEntity name k = getEntity (toSqlKey k) >>= \case
    Nothing -> logThrowM $ MissingEntity name k
    Just ent -> return ent

validateKey
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , SqlRecord r
       , ToBackendKey SqlBackend r
       )
    => Text -> Int64 -> Transaction m (Key r)
validateKey name = fmap entityKey . validateEntity name

validateUniqEntity
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique r)
       , SqlRecord r
       )
    => Text -> Unique r -> Transaction m (Entity r)
validateUniqEntity name uniq = getBy uniq >>= \case
    Nothing -> logThrowM $ MissingUniqEntity name uniq
    Just ent -> return ent

validateUniqKey
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique r)
       , SqlRecord r
       )
    => Text -> Unique r -> Transaction m (Key r)
validateUniqKey name = fmap entityKey . validateUniqEntity name

getUniq
    :: (MonadSql m, SqlRecord record, OnlyOneUniqueKey record)
    => record -> Transaction m (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq
    :: (MonadLogger m, MonadSql m, SqlRecord record, AtLeastOneUniqueKey record, Eq record, Show record)
    => record -> Transaction m ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> Log.logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

fieldFromEntity :: PersistEntity r => EntityField r v -> Entity r -> v
fieldFromEntity field = view (Sqlite.fieldLens field)

-- Generalisations
deleteBy :: (MonadSql m, SqlRecord rec) => Unique rec -> Transaction m ()
deleteBy = Transaction . Sqlite.deleteBy

deleteWhere :: (MonadSql m, SqlRecord rec) => [Filter rec] -> Transaction m ()
deleteWhere = Transaction . Sqlite.deleteWhere

get :: (MonadSql m, SqlRecord rec) => Key rec -> Transaction m (Maybe rec)
get = Transaction . Sqlite.get

getBy
    :: (MonadSql m, SqlRecord rec)
    => Unique rec -> Transaction m (Maybe (Entity rec))
getBy = Transaction . Sqlite.getBy

getEntity
    :: (MonadSql m, SqlRecord rec)
    => Key rec -> Transaction m (Maybe (Entity rec))
getEntity = Transaction . Sqlite.getEntity

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> Transaction m rec
getJust = Transaction . Sqlite.getJust

getJustEntity
    :: (MonadSql m, SqlRecord rec) => Key rec -> Transaction m (Entity rec)
getJustEntity = Transaction . Sqlite.getJustEntity

insert :: (MonadSql m, SqlRecord rec) => rec -> Transaction m (Key rec)
insert = Transaction . Sqlite.insert

insert_ :: (MonadSql m, SqlRecord rec) => rec -> Transaction m ()
insert_ = Transaction . Sqlite.insert_

insertBy
    :: (AtLeastOneUniqueKey rec, MonadSql m, SqlRecord rec)
    => rec -> Transaction m (Either (Entity rec) (Key rec))
insertBy = Transaction . Sqlite.insertBy

onlyUnique
    :: (MonadSql m, OnlyOneUniqueKey rec, SqlRecord rec)
    => rec -> Transaction m (Unique rec)
onlyUnique = Transaction . Sqlite.onlyUnique

rawExecute :: MonadSql m => Text -> [PersistValue] -> Transaction m ()
rawExecute query args = Transaction $ Sqlite.rawExecute query args

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m (Maybe (Entity rec))
selectFirst filters select = Transaction $ Sqlite.selectFirst filters select

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m [Key rec]
selectKeysList filters = Transaction . Sqlite.selectKeysList filters

selectKeys
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT a (Key rec) (Transaction m) ()
selectKeys filters select = toProducer $ Sqlite.selectKeys filters select

selectList
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m [Entity rec]
selectList filters select = Transaction $ Sqlite.selectList filters select

selectSource
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT a (Entity rec) (Transaction m) ()
selectSource filters select = toProducer $ Sqlite.selectSource filters select

count :: (MonadSql m, SqlRecord rec) => [Filter rec] -> Transaction m Int
count = Transaction . Sqlite.count

update
    :: (MonadSql m, SqlRecord rec)
    => Key rec -> [Update rec] -> Transaction m ()
update key = Transaction . Sqlite.update key

updateWhere
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [Update rec] -> Transaction m ()
updateWhere filts  = Transaction . Sqlite.updateWhere filts

likeFilter :: PersistField v => EntityField record v -> Text -> Filter record
likeFilter field val = Filter field filterVal backendFilter
  where
    filterVal = UnsafeValue $ T.concat ["%", val, "%"]
    backendFilter = BackendSpecificFilter "like"

whenNotExists
    :: (MonadResource m, MonadSql m, SqlRecord record)
    => [Filter record] -> ConduitT i a m () -> ConduitT i a m ()
whenNotExists filters act =
    runTransaction (selectFirst filters []) >>= \case
        Just _ -> return ()
        Nothing -> act

queryExternalImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> Transaction m (IntMap ExternalImpl)
queryExternalImplementations algoId = runConduit $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> Transaction m (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduit $
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

newtype Avg = Avg { getAvg :: Int } deriving (Show, Eq, Ord)
newtype Max = Max { getMax :: Int } deriving (Show, Eq, Ord)

getFieldLength
    :: forall a m rec
     . (MonadQuery m, SqlRecord rec)
    => EntityField rec a -> m (Avg, Max)
getFieldLength entityField = do
    queryText <- runTransaction $ do
        table <- Transaction $ getTableName (undefined :: rec)
        field <- Transaction $ getFieldName entityField
        return [i|
SELECT IFNULL(ROUND(AVG(length(#{table}.#{field}))), 0)
     , IFNULL(MAX(length(#{table}.#{field})), 0)
FROM #{table}
|]
    runSqlQuerySingle Query{..}
  where
    queryName :: Text
    queryName = "getMaxFieldQuery"

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n (Avg, Max)
    convert [avgPersistVal, maxPersistVal]
        | Right avgVal <- fromPersistValue avgPersistVal
        , Right maxVal <- fromPersistValue maxPersistVal
        = return (Avg avgVal, Max maxVal)
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlInt64]