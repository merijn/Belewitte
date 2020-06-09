{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Query.Field
    ( getDistinctFieldQuery
    , getDistinctFieldLikeQuery
    , getDistinctAlgorithmVersionQuery
    ) where

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
    return Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "distinctFieldQuery"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    converter
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n a
    converter [v] | Right val <- Sqlite.fromPersistValue v = return val
    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
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
    return Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "distinctFieldLikeQuery"

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue txt]

    converter
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n a
    converter [v] | Right val <- Sqlite.fromPersistValue v = return val
    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [Sqlite.sqlType (Proxy :: Proxy a)]

getDistinctAlgorithmVersionQuery
    :: Key Algorithm -> Maybe Text -> Query CommitId
getDistinctAlgorithmVersionQuery algoId prefix = Query
  { queryName = "distinctAlgorithmVersionQuery"
  , commonTableExpressions = []
  , params =
        [ toPersistValue algoId
        , toPersistValue prefix
        , toPersistValue prefix
        ]
  , queryText = [i|
SELECT DISTINCT algorithmVersion
FROM RunConfig
WHERE algorithmId = ? AND (algorithmVersion LIKE (? || '%') OR ? IS NULL)
|]
  , convert = Simple converter
  , ..
  }
  where
    converter
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n CommitId
    converter [v] | Right val <- Sqlite.fromPersistValue v = return val
    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [Sqlite.sqlType (Proxy :: Proxy CommitId)]
