{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Query
    ( MonadQuery
    , Query(..)
    , VariantInfo(..)
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQueryConduit
    , runSqlQueryCount
    , getDistinctFieldQuery
    , variantInfoQuery
    ) where

import Control.Monad (void)
import Data.Acquire (allocateAcquire)
import Control.Monad.Trans.Resource (release)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.Combinators as C
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Database.Persist.Sqlite
import Numeric (showGFloat)
import System.IO (Handle)

import Core
import Schema
import Sql (MonadSql, SqlRecord)
import Utils.Vector (byteStringToVector)

type MonadQuery m =
    ( MonadSql m
    , MonadExplain m
    , MonadLogger m
    , MonadResource m
    , MonadThrow m
    )

data Query r =
  Query
    { commonTableExpressions :: [Text]
    , cteParams :: [PersistValue]
    , params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadLogger m, MonadThrow m)
              => [PersistValue] -> m r
    , queryText :: Text
    , isExplain :: Bool
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

printExplainTree
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Handle -> ConduitT (Int64, Int64, Text) Void m ()
printExplainTree hnd= do
    liftIO . T.hPutStrLn hnd $ "QUERY PLAN"
    void $ C.foldM renderTree [0]
  where
    renderTree
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [Int64] -> (Int64, Int64, Text) -> m [Int64]
    renderTree [] _ = logThrowM $ QueryPlanUnparseable
    renderTree stack@(parent:parents) node@(parentId, nodeId, plan)
      | parent /= parentId = renderTree parents node
      | otherwise = liftIO $ do
          T.hPutStrLn hnd $ branches <> "--" <> plan
          return $ nodeId:stack
      where
        branches = mconcat $ intersperse "  " $ replicate (length stack) "|"

explainSqlQuery :: MonadQuery m => Query r -> Handle -> m ()
explainSqlQuery originalQuery hnd = do
    liftIO $ do
        T.hPutStrLn hnd $ T.replicate 80 "#"
        T.hPutStrLn hnd $ toQueryText originalQuery
        T.hPutStrLn hnd $ ""

    runConduit $ runSqlQuery' explainQuery .| printExplainTree hnd
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

    explainQuery = originalQuery { convert = explain, isExplain = True }

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

runSqlQuery :: MonadQuery m => Query r -> ConduitT () r m ()
runSqlQuery query = do
    logQueryExplanation $ explainSqlQuery query
    (timing, ()) <- withTime $ runSqlQuery' query

    let formattedTime :: Text
        formattedTime = T.pack $ showGFloat (Just 3) timing "s"

    logInfoN $ "Query time: " <> formattedTime

    logQueryExplanation $ \hnd -> liftIO $ do
        T.hPutStrLn hnd $ "\nQuery time: " <> formattedTime
        T.hPutStrLn hnd $ ""
        T.hPutStrLn hnd $ T.replicate 80 "#"

runSqlQueryConduit :: MonadQuery m => Query r -> ConduitT r Void m a -> m a
runSqlQueryConduit query@Query{..} sink = do
    srcRes <- liftPersist $ rawQueryRes (toQueryText query) queryParams
    (key, src) <- allocateAcquire srcRes
    runConduit (src .| C.mapM convert .| sink) <* release key
  where
    queryParams = cteParams ++ params

toQueryText :: Query r -> Text
toQueryText Query{..} = mconcat $
    [ mIf isExplain "EXPLAIN QUERY PLAN "
    , mIf (not $ null commonTableExpressions) "\nWITH"
    ] <> intersperse ",\n\n" commonTableExpressions <> ["\n", queryText]

runSqlQuery'
    :: (MonadLogger m, MonadResource m, MonadSql m, MonadThrow m)
    => Query r -> ConduitT () r m ()
runSqlQuery' query@Query{convert,cteParams,params} = do
    rawQuery (toQueryText query) queryParams .| C.mapM convert
  where
    queryParams = cteParams ++ params

runSqlQueryCount :: MonadQuery m => Query r -> m Int
runSqlQueryCount originalQuery = do
    result <- runConduit $ runSqlQuery countQuery .| await
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
     . (MonadSql m, PersistFieldSql a, SqlRecord rec)
     => EntityField rec a
     -> m (Query a)
getDistinctFieldQuery entityField = liftPersist $ do
    table <- getTableName (undefined :: rec)
    field <- getFieldName entityField
    let queryText = [i|SELECT DISTINCT #{table}.#{field} FROM #{table}|]
    return Query{..}
  where
    isExplain :: Bool
    isExplain = False

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n) => [PersistValue] -> n a
    convert [v] | Right val <- fromPersistValue v = return val
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [sqlType (Proxy :: Proxy a)]

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector (Int64, Double))
    } deriving (Show)

variantInfoQuery :: Key Algorithm -> Key Platform -> Query VariantInfo
variantInfoQuery algoId platformId = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    convert [ PersistInt64 (toSqlKey -> variantId)
            , stepOptimal
            , PersistDouble variantBestNonSwitching
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ]

            | PersistDouble variantOptimal <- stepOptimal
            = return VariantInfo{..}

            | PersistNull <- stepOptimal
            = let variantOptimal = infinite in return VariantInfo{..}
            where
              !infinite = 1/0
              variantTimings = VU.zip impls timings

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlReal, SqlReal, SqlBlob, SqlBlob]

    cteParams :: [PersistValue]
    cteParams = [toPersistValue algoId]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
          FROM (SELECT id AS implId
                     , type
                  FROM Implementation
                 WHERE algorithmId = ?)
         ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    params :: [PersistValue]
    params = [ toPersistValue platformId, toPersistValue platformId
             , toPersistValue algoId
             ]

    queryText = [i|
SELECT Variant.id
     , OptimalStep.optimal
     , Total.bestNonSwitching
     , ImplVector.impls
     , Total.timings
FROM Variant
LEFT JOIN
(   SELECT variantId
         , SUM(Step.minTime) AS optimal
    FROM (
        SELECT variantId
             , stepId
             ,  MIN(CASE Implementation.type
                    WHEN "Core" THEN avgTime
                    ELSE NULL END) AS minTime
        FROM StepTimer
        INNER JOIN Implementation
        ON StepTimer.implId = Implementation.id
        WHERE platformId = ?
        GROUP BY variantId, stepId
    ) AS Step
    GROUP BY variantId
) AS OptimalStep
ON Variant.id = OptimalStep.variantId

INNER JOIN
(   SELECT variantId
         , MIN(CASE IndexedImpls.type
               WHEN "Core" THEN avgTime
               ELSE NULL END) AS bestNonSwitching
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
      FROM TotalTimer
      INNER JOIN IndexedImpls
      ON TotalTimer.implId = IndexedImpls.implId
      WHERE platformId = ? AND TotalTimer.name = "computation"
      GROUP BY variantId
) AS Total
ON Variant.id = Total.variantId

LEFT JOIN ImplVector

WHERE Variant.algorithmId = ?
ORDER BY Variant.id ASC|]
