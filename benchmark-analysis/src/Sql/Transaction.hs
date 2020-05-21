{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Transaction (module Sql, module Sql.Transaction) where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (await)
import qualified Data.Conduit.Combinators as C
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.String.Interpolate.IsString (i)
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
import Query (CTE, Converter(Simple), MonadConvert, MonadQuery, Query(..))
import qualified Query
import Schema
import Sql.Core as Sql hiding (selectKeysRegion, selectSourceRegion)

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
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord record, AtLeastOneUniqueKey record, Eq record, Show record)
    => record -> Transaction m (Key record)
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity key r)
            | record == r -> return key
            | otherwise -> logThrowM $
                UniqueViolation (T.pack $ show record) (T.pack $ show r)
        Right key -> return key

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

selectSingleMaybe
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> Transaction m (Maybe (Entity rec))
selectSingleMaybe filters = Sql.selectSource filters [] $ do
    result <- await
    check <- await
    case check of
        Nothing -> return result
        Just _ -> logThrowM . ExpectedSingleValue $ ""

selectSingle
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> Transaction m (Entity rec)
selectSingle = selectSingleMaybe >=> \case
    Nothing -> logThrowM QueryReturnedZeroResults
    Just v -> return v

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m (Maybe (Entity rec))
selectFirst filters select = Transaction $ Sqlite.selectFirst filters select

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m [Key rec]
selectKeysList filters = Transaction . Sqlite.selectKeysList filters

selectList
    :: (MonadResource m, MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> Transaction m [Entity rec]
selectList filters select = Transaction $ Sqlite.selectList filters select

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

prefixFilter :: PersistField v => EntityField record v -> Text -> Filter record
prefixFilter field val = Filter field filterVal backendFilter
  where
    filterVal = UnsafeValue $ T.concat [val, "%"]
    backendFilter = BackendSpecificFilter "like"

queryExternalImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> Transaction m (IntMap ExternalImpl)
queryExternalImplementations algoId = selectImpls algoId $ C.foldMap toIntMap
  where
    selectImpls aId = Sql.selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> Transaction m (IntMap Implementation)
queryImplementations algoId = IM.union builtinImpls <$>
    selectImpls algoId (C.foldMap toIntMap)
  where
    selectImpls aId = Sql.selectSource [ ImplementationAlgorithmId ==. aId ] []

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
    Query.runSqlQuerySingle Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "getMaxFieldQuery"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    converter :: MonadConvert n => [PersistValue] -> n (Avg, Max)
    converter [avgPersistVal, maxPersistVal]
        | Right avgVal <- fromPersistValue avgPersistVal
        , Right maxVal <- fromPersistValue maxPersistVal
        = return (Avg avgVal, Max maxVal)
    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlInt64]
