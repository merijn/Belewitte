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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Query
    ( MonadQuery
    , Query(..)
    , VariantInfo(..)
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQuerySingleMaybe
    , runSqlQuerySingle
    , runSqlQueryConduit
    , runSqlQueryCount
    , getDistinctFieldQuery
    , variantInfoQuery
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
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Database.Persist.Sqlite as Sqlite
import Numeric (showGFloat)

import Core
import Schema
import Sql.Core (MonadSql, PersistFieldSql, SqlRecord, Transaction(..))
import qualified Sql.Core as Sql
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

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
    void (runRawSqlQuery Explain id explainQuery)
    .| renderExplainTree
    .| C.fold
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

runSqlQuery :: MonadQuery m => Query r -> ConduitT a r m ()
runSqlQuery = toProducer . runLoggingSqlQuery id

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
runSqlQueryConduit query sink =
  runLoggingSqlQuery (\src -> runConduit $ src .| sink) query

runLoggingSqlQuery
    :: ( MonadLogger m
       , MonadResource m
       , MonadThrow m
       , MonadExplain n
       , MonadLogger n
       , MonadSql n
       , MonadThrow n
       )
    => (ConduitT () r m () -> n a) -> Query r -> n a
runLoggingSqlQuery f query = do
    (formattedTime, result) <- runRawSqlQuery NoExplain f query

    shouldExplain <- shouldExplainQuery (queryName query)

    when shouldExplain $ do
        logQuery (queryName query) . mconcat $
            [ toQueryText query
            , "\n"
            , queryName query <> " time: " <> formattedTime
            , "\n"
            ]

        explainSqlQuery query >>= logExplain (queryName query)

    return result

runRawSqlQuery
    :: ( MonadLogger m, MonadResource m, MonadThrow m
       , MonadLogger n, MonadResource n, MonadSql n)
    => Explain -> (ConduitT () r m () -> n a) -> Query r -> n (Text, a)
runRawSqlQuery isExplain f query@Query{convert,cteParams,params} = do
    srcRes <- Sql.conduitQuery queryText queryParams
    (key, src) <- allocateAcquire srcRes
    (timing, r) <- withTime $ f (src .| C.mapM convert) <* release key

    let formattedTime :: Text
        formattedTime = T.pack $ showGFloat (Just 3) timing "s"

    logInfoN $ queryName query <> " time: " <> formattedTime
    return (formattedTime, r)
  where
    queryText = explainPrefix <> toQueryText query
    queryParams = cteParams ++ params

    explainPrefix = case isExplain of
        Explain -> "EXPLAIN QUERY PLAN "
        NoExplain -> mempty

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

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    , variantExternalTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

variantInfoQuery :: Key Algorithm -> Key Platform -> Query VariantInfo
variantInfoQuery algoId platformId = Query{..}
  where
    queryName :: Text
    queryName = "variantInfoQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    convert [ PersistInt64 (toSqlKey -> variantId)
            , stepOptimal
            , PersistDouble variantBestNonSwitching
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            , externalImpls
            , externalTimings
            ]

            | PersistDouble variantOptimal <- stepOptimal
            , Just variantExternalTimings <- maybeExternalTimings
            = return VariantInfo{..}

            | PersistNull <- stepOptimal
            , Just variantExternalTimings <- maybeExternalTimings
            = let variantOptimal = infinity in return VariantInfo{..}
      where
        !infinity = 1/0
        variantTimings = VS.zipWith ImplTiming impls timings

        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case (externalImpls, externalTimings) of
            (PersistNull, PersistNull) -> Just VS.empty
            (PersistNull, PersistByteString _) -> Nothing
            (PersistByteString extImpls, PersistNull) ->
                Just $ VS.map (\impl -> ImplTiming impl infinity)
                              (byteStringToVector extImpls)
            (PersistByteString extImpls, PersistByteString times) ->
                Just $ VS.zipWith ImplTiming (byteStringToVector extImpls)
                                             (byteStringToVector times)
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlReal, SqlReal, SqlBlob, SqlBlob, SqlBlob, SqlBlob]

    cteParams :: [PersistValue]
    cteParams = [toPersistValue algoId, toPersistValue algoId]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY id)
             , id
             , type
        FROM Implementation
        WHERE algorithmId = ?
        ORDER BY id
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    ),

    IndexedExternalImpls(idx, implId) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
          FROM (SELECT id AS implId
                  FROM ExternalImpl
                 WHERE algorithmId = ?)
         ORDER BY implId
    ),

    ExternalImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedExternalImpls))
          FROM IndexedExternalImpls
    )|]]

    params :: [PersistValue]
    params = [ toPersistValue platformId, toPersistValue algoId
             , toPersistValue platformId
             ]

    queryText = [i|
SELECT OptimalStep.variantId
     , OptimalStep.optimal
     , Total.bestNonSwitching
     , ImplVector.impls
     , Total.timings
     , ExternalImplVector.impls
     , External.timings
FROM RunConfig

LEFT JOIN
(   SELECT runConfigId
         , variantId
         , SUM(Step.minTime) AS optimal
    FROM (
        SELECT Run.runConfigId
             , Run.variantId
             , stepId
             , MIN(avgTime) FILTER (WHERE type == 'Core') AS minTime
        FROM StepTimer

        INNER JOIN Run
        ON StepTimer.runId = Run.id

        INNER JOIN Implementation
        ON Run.implId = Implementation.id

        GROUP BY Run.runConfigId, Run.variantId, stepId
    ) AS Step
    GROUP BY runConfigId, variantId
) AS OptimalStep
ON RunConfig.id = OptimalStep.runConfigId

INNER JOIN
(   SELECT Run.variantId
         , MIN(avgTime) FILTER (WHERE type == 'Core') AS bestNonSwitching
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
      FROM TotalTimer

      INNER JOIN Run
      ON TotalTimer.runId = Run.id

      INNER JOIN IndexedImpls
      ON Run.implId = IndexedImpls.implId

      WHERE TotalTimer.name = 'computation'
      GROUP BY Run.variantId
) AS Total
ON OptimalStep.variantId = Total.variantId

LEFT JOIN
(   SELECT variantId
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedExternalImpls))
           AS timings
      FROM ExternalTimer
      INNER JOIN IndexedExternalImpls
      ON ExternalTimer.implId = IndexedExternalImpls.implId
      WHERE platformId = ? AND ExternalTimer.name = 'computation'
      GROUP BY variantId
) AS External
ON OptimalStep.variantId = External.variantId

LEFT JOIN ImplVector
LEFT JOIN ExternalImplVector

WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
ORDER BY OptimalStep.variantId ASC|]
