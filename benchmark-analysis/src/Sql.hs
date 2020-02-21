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
    , module SqlCore
    , module Sql
    ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Conduit (ConduitT, Void, transPipe)
import Data.IntMap (IntMap)
import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)
import Database.Persist.Sqlite (sqlType)

import Core
import Query (CTE, MonadQuery, Query(..), runSqlQuerySingleMaybe)
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
    run Query{..}
  where
    queryName :: Text
    queryName = "getGlobalVar"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue (showText var) ]

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n a
    convert [sqlResult]
        | Right result <- fromPersistValue sqlResult
        = return result

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ sqlType (Proxy :: Proxy a) ]

getGlobalVar :: (MonadQuery m, PersistFieldSql a) => GlobalVar a -> m (Maybe a)
getGlobalVar = rawGetGlobalVar runSqlQuerySingleMaybe

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
validateEntity name k = runTransaction $ SqlTrans.validateEntity name k

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
    runTransaction $ SqlTrans.validateUniqEntity name uniq

validateUniqKey
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , Show (Unique r)
       , SqlRecord r
       )
    => Text -> Unique r -> m (Key r)
validateUniqKey name = fmap entityKey . validateUniqEntity name

getBy :: (MonadSql m, SqlRecord rec) => Unique rec -> m (Maybe (Entity rec))
getBy = runTransaction . SqlTrans.getBy

getEntity :: (MonadSql m, SqlRecord rec) => Key rec -> m (Maybe (Entity rec))
getEntity = runTransaction . SqlTrans.getEntity

getJust :: (MonadSql m, SqlRecord rec) => Key rec -> m rec
getJust = runTransaction . SqlTrans.getJust

getJustEntity :: (MonadSql m, SqlRecord rec) => Key rec -> m (Entity rec)
getJustEntity = runTransaction . SqlTrans.getJustEntity

insert :: (MonadSql m, SqlRecord rec) => rec -> m (Key rec)
insert = runTransaction . SqlTrans.insert

insert_ :: (MonadSql m, SqlRecord rec) => rec -> m ()
insert_ = runTransaction . SqlTrans.insert_

selectSingleMaybe
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> m (Maybe (Entity rec))
selectSingleMaybe = runTransaction . SqlTrans.selectSingleMaybe

selectSingle
    :: (MonadLogger m, MonadSql m, MonadThrow m, SqlRecord rec)
    => [Filter rec] -> m (Entity rec)
selectSingle = runTransaction . SqlTrans.selectSingle

selectFirst
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m (Maybe (Entity rec))
selectFirst filters select =
    runTransaction $ SqlTrans.selectFirst filters select

selectKeys
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT (Key rec) Void m r
    -> m r
selectKeys filters select sink = runTransaction $
    SqlTrans.selectKeys filters select (transPipe lift sink)

selectKeysList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Key rec]
selectKeysList filters = runTransaction . SqlTrans.selectKeysList filters

selectList
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec] -> [SelectOpt rec] -> m [Entity rec]
selectList filters select = runTransaction $ SqlTrans.selectList filters select

selectSource
    :: (MonadSql m, SqlRecord rec)
    => [Filter rec]
    -> [SelectOpt rec]
    -> ConduitT (Entity rec) Void m r
    -> m r
selectSource filters select sink = runTransaction $
    SqlTrans.selectSource filters select (transPipe lift sink)

update :: (MonadSql m, SqlRecord rec) => Key rec -> [Update rec] -> m ()
update key = runTransaction . SqlTrans.update key

updateWhere
    :: (MonadSql m, SqlRecord rec) => [Filter rec] -> [Update rec] -> m ()
updateWhere filts = runTransaction . SqlTrans.updateWhere filts

queryExternalImplementations
    :: MonadSql m => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId =
    runTransaction $ SqlTrans.queryExternalImplementations algoId

queryImplementations
    :: MonadSql m => Key Algorithm -> m (IntMap Implementation)
queryImplementations algoId =
    runTransaction $ SqlTrans.queryImplementations algoId
