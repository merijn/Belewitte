{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Query
    ( MonadQuery
    , Query(..)
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQuerySingleMaybe
    , runSqlQuerySingle
    , runSqlQueryConduit
    , runSqlQueryCount
    , getDistinctFieldQuery
    ) where

import Control.Monad ((>=>), void, when)
import Data.Acquire (allocateAcquire)
import Control.Monad.Trans.Resource (release)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit, toProducer, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C hiding (fold, mapM)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite
import Numeric (showGFloat)

import Core
import Schema
import Sql.Core (MonadSql, PersistFieldSql, SqlRecord, Transaction(..))
import qualified Sql.Core as Sql

type MonadQuery m =
    (MonadResource m, MonadSql m, MonadExplain m, MonadLogger m, MonadThrow m)

data Explain = Explain | NoExplain

data Query r =
  Query
    { queryName :: Text
    , commonTableExpressions :: [Text]
    , cteParams :: [PersistValue]
    , params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadLogger m, MonadThrow m)
              => [PersistValue] -> m r
    , queryText :: Text
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

toQueryText :: Query r -> Text
toQueryText Query{..} = mconcat $
    [ mIf hasCTEs "\nWITH" ] <> intersperse ",\n\n" commonTableExpressions <>
    [ mIf hasCTEs "\n", queryText ]
  where
    hasCTEs = not $ null commonTableExpressions

renderExplainTree
    :: (MonadLogger m, MonadThrow m) => ConduitT (Int64, Int64, Text) Text m ()
renderExplainTree = void $ do
    yield "\nQUERY PLAN\n"
    C.mapAccumM renderTree [0]
  where
    renderTree
        :: (MonadLogger m, MonadThrow m)
        => (Int64, Int64, Text) -> [Int64] -> m ([Int64], Text)
    renderTree _ [] = logThrowM $ QueryPlanUnparseable
    renderTree node@(parentId, nodeId, plan) stack@(parent:parents)
      | parent /= parentId = renderTree node parents
      | otherwise = return (nodeId:stack, branches <> "--" <> plan <> "\n")
      where
        branches = mconcat $ intersperse "  " $ replicate (length stack) "|"

explainSqlQuery :: MonadQuery m => Query r -> m Text
explainSqlQuery originalQuery = runConduit $
    runRawSqlQuery Explain explainQuery $ renderExplainTree .| C.fold
  where
    explain
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Int64, Int64, Text)
    explain
        [ PersistInt64 nodeId
        , PersistInt64 parentId
        , PersistInt64 _
        , PersistText t
        ] = return (parentId, nodeId, t)
    explain actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlInt64, SqlInt64, SqlString]

    explainQuery = originalQuery { convert = explain }

randomizeQuery :: Int -> Int -> Query r -> (Query r, Query r)
randomizeQuery seed trainingSize originalQuery = (training, validation)
  where
    randomizedQuery =
      [i|SELECT * FROM (#{queryText originalQuery}) ORDER BY random(?) |]

    extraParams = [toPersistValue seed, toPersistValue trainingSize]

    training = originalQuery
      { params = params originalQuery ++ extraParams
      , queryText = randomizedQuery <> [i|LIMIT ?|]
      }

    validation = originalQuery
      { params = params originalQuery ++ extraParams
      , queryText = randomizedQuery <> [i|LIMIT -1 OFFSET ?|]
      }

runSqlQuery :: MonadQuery m => Query r -> ConduitT r o m a -> ConduitT i o m a
runSqlQuery query sink = runLoggingSqlQuery query sink

runSqlQuerySingleMaybe :: MonadQuery m => Query r -> m (Maybe r)
runSqlQuerySingleMaybe query = runSqlQueryConduit query $ do
    result <- await
    check <- await
    case check of
        Nothing -> return result
        Just _ -> logThrowM $ ExpectedSingleValue (toQueryText query)

runSqlQuerySingle :: MonadQuery m => Query r -> m r
runSqlQuerySingle = runSqlQuerySingleMaybe >=> \case
    Nothing -> logThrowM QueryReturnedZeroResults
    Just v -> return v

runSqlQueryConduit :: MonadQuery m => Query r -> ConduitT r Void m a -> m a
runSqlQueryConduit query sink = runConduit $ runLoggingSqlQuery query sink

runLoggingSqlQuery
    :: ( MonadExplain m
       , MonadLogger m
       , MonadResource m
       , MonadSql m
       , MonadThrow m
       )
    => Query r -> ConduitT r o m a -> ConduitT i o m a
runLoggingSqlQuery query sink = do
    shouldExplain <- shouldExplainQuery (queryName query)

    when shouldExplain $ do
        logQuery (queryName query) . mconcat $
            [ toQueryText query
            , "\n"
            ]

        explainSqlQuery query >>= logExplain (queryName query)

    runRawSqlQuery NoExplain query sink

runRawSqlQuery
    :: (MonadLogger m, MonadResource m, MonadSql m, MonadThrow m)
    => Explain -> Query r -> ConduitT r o m a -> ConduitT i o m a
runRawSqlQuery isExplain query@Query{convert,cteParams,params} sink = do
    srcRes <- Sql.conduitQuery queryText queryParams
    (key, src) <- allocateAcquire srcRes
    (timing, r) <- withTime $ toProducer src .| C.mapM convert .| sink
    release key

    let formattedTime :: Text
        formattedTime = T.pack $ showGFloat (Just 3) timing "s"

    logInfoN $ queryName query <> " time: " <> formattedTime
    return r
  where
    queryText = explainPrefix <> toQueryText query
    queryParams = cteParams ++ params

    explainPrefix = case isExplain of
        Explain -> "EXPLAIN QUERY PLAN "
        NoExplain -> mempty

runSqlQueryCount :: MonadQuery m => Query r -> m Int
runSqlQueryCount originalQuery = do
    result <- runSqlQueryConduit countQuery await
    case result of
        Just n -> return n
        Nothing -> logThrowM QueryReturnedZeroResults
  where
    countQuery = originalQuery
        { queryText =
            "SELECT COUNT(*) FROM (" <> queryText originalQuery <> ")"
        , convert = \case
            [PersistInt64 n] -> return $ fromIntegral n
            actualValues -> logThrowM $ QueryResultUnparseable actualValues
                [SqlInt64]
        }

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
    queryName = "getDistinctFieldQuery"

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n) => [PersistValue] -> n a
    convert [v] | Right val <- Sqlite.fromPersistValue v = return val
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [Sqlite.sqlType (Proxy :: Proxy a)]
