{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module FieldQuery (getDistinctFieldQuery, getDistinctFieldLikeQuery) where

import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)
import qualified Database.Persist.Sqlite as Sqlite

import Core
import Query
import Schema
import Sql.Core (MonadSql, PersistFieldSql, Transaction(..), SqlRecord)
import qualified Sql.Core as Sql

getDistinctFieldQuery
    :: forall a m rec
     . (MonadResource m, MonadSql m, PersistFieldSql a, SqlRecord rec)
     => EntityField rec a
     -> m (Query a)
getDistinctFieldQuery entityField = Sql.runTransaction $ do
    table <- Transaction $ Sqlite.getTableName (undefined :: rec)
    field <- Transaction $ Sqlite.getFieldName entityField
    let queryText = [i|SELECT DISTINCT #{table}.#{field} FROM #{table}|]
    return Query{..}
  where
    queryName :: Text
    queryName = "distinctFieldQuery"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n) => [PersistValue] -> n a
    convert [v] | Right val <- Sqlite.fromPersistValue v = return val
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [Sqlite.sqlType (Proxy :: Proxy a)]

getDistinctFieldLikeQuery
    :: forall a m rec
     . (MonadResource m, MonadSql m, PersistFieldSql a, SqlRecord rec)
     => EntityField rec a
     -> Text
     -> m (Query a)
getDistinctFieldLikeQuery entityField txt = Sql.runTransaction $ do
    table <- Transaction $ Sqlite.getTableName (undefined :: rec)
    field <- Transaction $ Sqlite.getFieldName entityField
    let queryText = [i|
SELECT DISTINCT #{table}.#{field}
FROM #{table}
WHERE #{table}.#{field} LIKE (? || '%')
|]
    return Query{..}
  where
    queryName :: Text
    queryName = "distinctFieldLikeQuery"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue txt]

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n) => [PersistValue] -> n a
    convert [v] | Right val <- Sqlite.fromPersistValue v = return val
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [Sqlite.sqlType (Proxy :: Proxy a)]
