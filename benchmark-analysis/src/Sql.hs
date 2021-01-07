{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql
    ( SqlTrans.Avg(..)
    , SqlTrans.Max(..)
    , SqlTrans.getFieldLength
    , SqlTrans.likeFilter
    , SqlTrans.prefixFilter
    , module SqlCore
    , module Sql
    ) where

import Data.Conduit (ConduitT, Void, transPipe)
import Data.IntMap (IntMap)
import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)
import Database.Persist.Sqlite (OnlyOneUniqueKey, sqlType)

import Core
import Query (CTE, Converter(Simple), MonadConvert, MonadQuery, Query(..))
import qualified Query
import Schema
import Schema.GlobalVars (Unique(UniqGlobal))
import Sql.Core as SqlCore hiding (selectKeys, selectSource)
import qualified Sql.Transaction as SqlTrans

rawGetGlobalVar
    :: forall a b m
     . (MonadQuery m, PersistFieldSql a)
    => (Query a -> m b) -> GlobalVar a -> m b
rawGetGlobalVar run var = do
    let queryText = [i|SELECT value FROM GlobalVars WHERE name = ?|]
    run Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "getGlobalVar"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue (showText var) ]

    converter :: MonadConvert n => [PersistValue] -> n a
    converter [sqlResult]
        | Right result <- fromPersistValue sqlResult
        = return result

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ sqlType (Proxy :: Proxy a) ]

getGlobalVar :: (MonadQuery m, PersistFieldSql a) => GlobalVar a -> m (Maybe a)
getGlobalVar = rawGetGlobalVar Query.runSqlQuerySingleMaybe

initialiseGlobalVar
    :: (MonadSql m, PersistFieldSql a) => GlobalVar a -> a -> m ()
initialiseGlobalVar var value = runTransaction $ SqlTrans.rawExecute query
    [ toPersistValue (show var), toPersistValue value ]
  where
    query = [i|INSERT INTO GlobalVars ("name","value") VALUES (?,?)|]

setGlobalVar
    :: (MonadSql m, PersistFieldSql a)
    => GlobalVar a -> a -> m ()
setGlobalVar var value = runTransaction $ SqlTrans.rawExecute query
    [ toPersistValue (show var), toPersistValue value ]
  where
    query = [i|INSERT OR REPLACE INTO GlobalVars ("name","value") VALUES (?,?)|]

unsetGlobalVar :: (MonadSql m) => GlobalVar a -> m ()
unsetGlobalVar var =
    runTransaction . SqlTrans.deleteBy $ UniqGlobal (showText var)

-- Wrapped re-exports
validateEntity
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , SqlRecord r
       , ToBackendKey SqlBackend r
       )
    => Text -> Int64 -> m (Entity r)
validateEntity name k = runReadOnlyTransaction $ SqlTrans.validateEntity name k

validateKey
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , SqlRecord r
       , ToBackendKey SqlBackend r
       )
    => Text -> Int64 -> m (Key r)
validateKey name = fmap entityKey . validateEntity name

validateUniqEntity
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique r)
       , SqlRecord r
       )
    => Text -> Unique r -> m (Entity r)
validateUniqEntity name uniq =
    runReadOnlyTransaction $ SqlTrans.validateUniqEntity name uniq

validateUniqKey
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique r)
       , SqlRecord r
       )
    => Text -> Unique r -> m (Key r)
validateUniqKey name = fmap entityKey . validateUniqEntity name

getJustBy
    :: (MonadLogger m, MonadSql m, MonadThrow m, Show (Unique rec), SqlRecord rec)
    => Unique rec -> m (Entity rec)
getJustBy k = do
    mVal <- getBy k
    case mVal of
        Just v -> return v
        Nothing -> logThrowM $ MissingUniqEntity "value" k

getJustKeyBy
    :: (MonadLogger m, MonadSql m, MonadThrow m, Show (Unique rec), SqlRecord rec)
    => Unique rec -> m (Key rec)
getJustKeyBy k = entityKey <$> getJustBy k

getBy :: (MonadSql m, SqlRecord rec) => Unique rec -> m (Maybe (Entity rec))
getBy = runReadOnlyTransaction . SqlTrans.getBy

getEntity :: (MonadSql m, SqlRecord rec) => Key rec -> m (Maybe (Entity rec))
getEntity = runReadOnlyTransaction . SqlTrans.getEntity

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> m rec
getJust = runReadOnlyTransaction . SqlTrans.getJust

getJustEntity :: (MonadSql m, SqlRecord rec) => Key rec -> m (Entity rec)
getJustEntity = runReadOnlyTransaction . SqlTrans.getJustEntity

insert :: (MonadSql m, SqlRecord rec) => rec -> m (Key rec)
insert = runTransaction . SqlTrans.insert

insert_ :: (MonadSql m, SqlRecord rec) => rec -> m ()
insert_ = runTransaction . SqlTrans.insert_

insertKey :: (MonadSql m, SqlRecord rec) => Key rec -> rec -> m ()
insertKey k = runTransaction . SqlTrans.insertKey k

onlyUnique
    :: (MonadSql m, OnlyOneUniqueKey rec, SqlRecord rec)
    => rec -> m (Unique rec)
onlyUnique = runReadOnlyTransaction . SqlTrans.onlyUnique

insertUniq
    :: ( MonadLogger m, MonadSql m, MonadThrow m, SqlRecord record
       , AtLeastOneUniqueKey record, Eq record, Show record
       )
    => record -> m (Key record)
insertUniq = runTransaction . SqlTrans.insertUniq

selectSingleMaybe
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> m (Maybe (Entity rec))
selectSingleMaybe = runReadOnlyTransaction . SqlTrans.selectSingleMaybe

selectSingle
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> m (Entity rec)
selectSingle = runReadOnlyTransaction . SqlTrans.selectSingle

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m (Maybe (Entity rec))
selectFirst filters select =
    runReadOnlyTransaction $ SqlTrans.selectFirst filters select

selectKeys
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT (Key rec) Void m r
    -> m r
selectKeys filters select sink = runReadOnlyTransaction $
    SqlTrans.selectKeys filters select (transPipe lift sink)

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Key rec]
selectKeysList filters =
  runReadOnlyTransaction . SqlTrans.selectKeysList filters

selectList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Entity rec]
selectList filters select =
  runReadOnlyTransaction $ SqlTrans.selectList filters select

selectSource
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT (Entity rec) Void m r
    -> m r
selectSource filters select sink = runReadOnlyTransaction $
    SqlTrans.selectSource filters select (transPipe lift sink)

update :: (MonadSql m, SqlRecord rec) => Key rec -> [Update rec] -> m ()
update key = runTransaction . SqlTrans.update key

updateWhere
    :: (MonadSql m, SqlRecord rec) => [Filter rec] -> [Update rec] -> m ()
updateWhere filts = runTransaction . SqlTrans.updateWhere filts

queryExternalImplementations
    :: MonadSql m => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId =
    runReadOnlyTransaction $ SqlTrans.queryExternalImplementations algoId

queryImplementations
    :: MonadSql m => Key Algorithm -> m (IntMap Implementation)
queryImplementations algoId =
    runReadOnlyTransaction $ SqlTrans.queryImplementations algoId
