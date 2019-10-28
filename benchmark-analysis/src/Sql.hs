{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql (module Sql.Core, module Sql) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Conduit (MonadIO, MonadResource, (.|), runConduit)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)

import Core
import Database.Persist.Sqlite
    (getFieldName, getTableName, liftPersist, rawExecute, sqlType)
import Query (MonadQuery, Query(..), runSqlQuerySingle, runSqlQuerySingleMaybe)
import Schema
import Schema.GlobalVars (Unique(UniqGlobal))
import Sql.Core hiding (executeSql, liftProjectPersist)

newtype Avg = Avg { getAvg :: Int } deriving (Show, Eq, Ord)
newtype Max = Max { getMax :: Int } deriving (Show, Eq, Ord)

validateEntity
    :: ( MonadLogger m
       , MonadSql m
       , MonadThrow m
       , SqlRecord r
       , ToBackendKey SqlBackend r
       )
    => Text -> Int64 -> m (Entity r)
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
    => Text -> Unique r -> m (Key r)
validateUniqKey name = fmap entityKey . validateUniqEntity name

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

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
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

getGlobalVar_ :: (MonadQuery m, PersistFieldSql a) => GlobalVar a -> m a
getGlobalVar_ = rawGetGlobalVar runSqlQuerySingle

initialiseGlobalVar
    :: (MonadSql m, PersistFieldSql a) => GlobalVar a -> a -> m ()
initialiseGlobalVar var value = liftPersist $ do
    rawExecute query [ toPersistValue (show var), toPersistValue value ]
  where
    query = [i|INSERT INTO GlobalVars ("name","value") VALUES (?,?)|]

setGlobalVar :: (MonadSql m, PersistFieldSql a) => GlobalVar a -> a -> m ()
setGlobalVar var value = liftPersist $ do
    rawExecute query [ toPersistValue (show var), toPersistValue value ]
  where
    query = [i|INSERT OR REPLACE INTO GlobalVars ("name","value") VALUES (?,?)|]

unsetGlobalVar :: MonadSql m => GlobalVar a -> m ()
unsetGlobalVar var = deleteBy $ UniqGlobal (showText var)

getFieldLength
    :: forall a m rec
     . (MonadQuery m, SqlRecord rec)
    => EntityField rec a -> m (Avg, Max)
getFieldLength entityField = do
    table <- liftPersist $ getTableName (undefined :: rec)
    field <- liftPersist $ getFieldName entityField
    let queryText = [i|
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

queryExternalImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId = runConduit $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> m (IntMap Implementation)
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
