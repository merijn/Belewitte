{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query
    ( Query
    , StepInfo(..)
    , VariantInfo(..)
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQueryCount
    , stepInfoQuery
    , variantInfoQuery
    , timePlotQuery
    , levelTimePlotQuery
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Resource (release)
import Data.Acquire (allocateAcquire)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.Combinators as C
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.String.Interpolate.IsString (i)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Database.Persist.Sqlite
import Numeric (showGFloat)
import System.IO (Handle)

import Core
import Schema
import Utils (byteStringToVector)

data Query r =
  Query
    { commonTableExpressions :: [Text]
    , params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadLogger m, MonadThrow m)
              => [PersistValue] -> m r
    , query :: Text
    , isExplain :: Bool
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

printExplainTree
    :: (MonadIO m, MonadThrow m)
    => Handle -> ConduitT (Int64, Int64, Text) Void m ()
printExplainTree hnd= do
    liftIO . T.hPutStrLn hnd $ "QUERY PLAN"
    void $ C.foldM renderTree [0]
  where
    renderTree
        :: (MonadIO m, MonadThrow m)
        => [Int64] -> (Int64, Int64, Text) -> m [Int64]
    renderTree [] _ = throwM $ Error "Something went horribly wrong!"
    renderTree stack@(parent:parents) node@(parentId, nodeId, plan)
      | parent /= parentId = renderTree parents node
      | otherwise = liftIO $ do
          T.hPutStrLn hnd $ branches <> "--" <> plan
          return $ nodeId:stack
      where
        branches = mconcat $ intersperse "  " $ replicate (length stack) "|"

explainSqlQuery :: Query r -> Handle -> SqlM ()
explainSqlQuery originalQuery hnd = do
    liftIO $ do
        T.hPutStrLn hnd $ T.replicate 80 "#"
        T.hPutStrLn hnd $ toTextQuery originalQuery
        T.hPutStrLn hnd $ ""

    runSqlQuery' explainQuery $ printExplainTree hnd
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
    explain _ = logThrowM $ Error "Explain failed!"

    explainQuery = originalQuery { convert = explain, isExplain = True }

randomizeQuery :: Int -> Integer -> Query r -> (Query r, Query r)
randomizeQuery seed trainingSize originalQuery = (training, validation)
  where
    randomizedQuery =
      [i|SELECT * FROM (#{query originalQuery}) ORDER BY random(#{seed}) |]

    training = originalQuery
      { query = randomizedQuery <> [i|LIMIT #{trainingSize}|] }

    validation = originalQuery
      { query = randomizedQuery <> [i|LIMIT -1 OFFSET #{trainingSize}|] }

runSqlQuery :: Query r -> ConduitT r Void SqlM a -> SqlM a
runSqlQuery query sink = do
    logQueryExplanation $ explainSqlQuery query
    (timing, result) <- withTime $ runSqlQuery' query sink

    let formattedTime :: Text
        formattedTime = T.pack $ showGFloat (Just 3) timing "s"

    logInfoN $ "Query time: " <> formattedTime

    logQueryExplanation $ \hnd -> do
        liftIO $ do
            T.hPutStrLn hnd $ "\nQuery time: " <> formattedTime
            T.hPutStrLn hnd $ ""
            T.hPutStrLn hnd $ T.replicate 80 "#"
    return result

mIf :: Monoid m => Bool -> m -> m
mIf condition val
    | condition = val
    | otherwise = mempty

toTextQuery :: Query r -> Text
toTextQuery Query{..} = mconcat $
    [ mIf isExplain "EXPLAIN QUERY PLAN "
    , mIf (not $ null commonTableExpressions) "\nWITH "
    ] <> intersperse ",\n\n" commonTableExpressions <> [query]

runSqlQuery' :: Query r -> ConduitT r Void SqlM a -> SqlM a
runSqlQuery' query@Query{convert,params} sink = do
    srcRes <- liftPersist $ rawQueryRes (toTextQuery query) params
    (key, src) <- allocateAcquire srcRes
    runConduit (src .| C.mapM convert .| sink) <* release key

runSqlQueryCount :: Query r -> SqlM Int
runSqlQueryCount originalQuery = do
    result <- runSqlQuery countQuery await
    case result of
        Just n -> return n
        Nothing -> logThrowM . Error $ "Missing count result!"
  where
    countQuery = originalQuery
        { query = "SELECT COUNT(*) FROM (" <> query originalQuery <> ")"
        , convert = \case
            [PersistInt64 n] -> return $ fromIntegral n
            _ -> logThrowM . Error $ "Unexpected value in count query"
        }

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector Double)
    , stepBestImpl :: {-# UNPACK #-} !Int64
    , stepVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , stepTimings :: {-# UNPACK #-} !(Vector (Int64, Double))
    } deriving (Show)

stepInfoQuery
    :: Key Algorithm -> Key GPU -> Set Text -> Set Text -> Query StepInfo
stepInfoQuery algoId gpuId graphProperties stepProperties = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    params :: [PersistValue]
    params = [toPersistValue gpuId, toPersistValue algoId]

    whereClauses :: Set Text -> Text
    whereClauses = T.intercalate " OR " . map clause . S.toAscList
      where
        clause t = [i|property = "#{t}"|]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m StepInfo
    convert [ PersistByteString (byteStringToVector -> graphProps)
            , PersistByteString (byteStringToVector -> rawStepProps)
            , PersistInt64 stepBestImpl
            , PersistInt64 (toSqlKey -> stepVariantId) , PersistInt64 stepId
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] = return $ StepInfo{..}
      where
        stepProps = graphProps <> rawStepProps
        stepTimings = VU.zip impls timings

    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedGraphProps(idx, property) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY property)
             , property
          FROM (SELECT DISTINCT property
                 FROM GraphProp
                WHERE #{whereClauses graphProperties})
         ORDER BY property
    ),

    IndexedStepProps(idx, property) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY property)
             , property
          FROM (SELECT DISTINCT property
                  FROM StepProp
                 WHERE #{whereClauses stepProperties})
         ORDER BY property
    ),

    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
          FROM (SELECT id AS implId
                     , type
                  FROM Implementation
                 WHERE algorithmId = #{fromSqlKey algoId})
         ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    query = [i|
SELECT GraphProps.props
     , StepProps.props
     , Step.implId
     , Variant.id
     , Step.stepId
     , ImplVector.impls
     , Step.timings
FROM Variant
INNER JOIN Graph
ON Variant.graphId = Graph.id

INNER JOIN
(   SELECT variantId, stepId, IndexedImpls.implId
         , MIN(CASE IndexedImpls.type
               WHEN "Core" THEN avgTime
               ELSE NULL END)
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
    FROM StepTimer
    INNER JOIN IndexedImpls
    ON StepTimer.implId = IndexedImpls.implId
    WHERE gpuId = ?
    GROUP BY variantId, stepId

) AS Step
ON Variant.id = Step.variantId

INNER JOIN
(   SELECT graphId
         , double_vector(value, idx, #{S.size graphProperties}) AS props
    FROM GraphProp
    INNER JOIN IndexedGraphProps AS IdxProps
    ON IdxProps.property = GraphProp.property
    GROUP BY graphId
) AS GraphProps
ON GraphProps.graphId = Graph.id

INNER JOIN
(   SELECT variantId, stepId
         , double_vector(value, idx, #{S.size stepProperties}) AS props
    FROM StepProp
    INNER JOIN IndexedStepProps AS IdxProps
    ON IdxProps.property = StepProp.property
    GROUP BY variantId, stepId
) AS StepProps
ON Variant.id = StepProps.variantId AND Step.stepId = StepProps.stepId

LEFT JOIN ImplVector
WHERE Variant.algorithmId = ?
ORDER BY Variant.id, Step.stepId ASC|]

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector (Int64, Double))
    } deriving (Show)

variantInfoQuery :: Key Algorithm -> Key GPU -> Query VariantInfo
variantInfoQuery algoId gpuId = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    params :: [PersistValue]
    params = [ toPersistValue gpuId, toPersistValue gpuId
             , toPersistValue algoId
             ]

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

    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
          FROM (SELECT id AS implId
                     , type
                  FROM Implementation
                 WHERE algorithmId = #{fromSqlKey algoId})
         ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    query = [i|
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
        WHERE gpuId = ?
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
      WHERE gpuId = ? AND TotalTimer.name = "computation"
      GROUP BY variantId
) AS Total
ON Variant.id = Total.variantId

LEFT JOIN ImplVector

WHERE Variant.algorithmId = ?
ORDER BY Variant.id ASC|]

timePlotQuery
    :: Key Algorithm
    -> Key GPU
    -> Set (Key Variant)
    -> Query (Text, Vector (Int64, Double))
timePlotQuery algoId gpuId variants = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    params :: [PersistValue]
    params = [toPersistValue gpuId, toPersistValue algoId]

    havingClause = T.intercalate " OR " . map clause . S.toList
      where
        clause k = [i|variantId = #{fromSqlKey k}|]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Text, Vector (Int64, Double))
    convert [ PersistText graph
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] = return $ (graph, VU.zip impls timings)
    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
          FROM (SELECT id AS implId
                     , type
                  FROM Implementation
                 WHERE algorithmId = #{fromSqlKey algoId})
         ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    query = [i|
SELECT Graph.name
     , ImplVector.impls
     , Total.timings
FROM Variant
INNER JOIN Graph
ON Variant.graphId = Graph.id

INNER JOIN
(   SELECT variantId
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
    FROM TotalTimer
    INNER JOIN IndexedImpls
    ON IndexedImpls.implId = TotalTimer.implId
    WHERE gpuId = ? AND name = "computation"
    GROUP BY variantId
    HAVING #{havingClause variants}
) AS Total
ON Variant.id = Total.variantId

LEFT JOIN ImplVector

WHERE Variant.name = "default" AND Variant.algorithmId = ?
ORDER BY Variant.id ASC|]

levelTimePlotQuery
    :: Key GPU -> Key Variant -> Query (Int64, Vector (Int64, Double))
levelTimePlotQuery gpuId variant = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    params :: [PersistValue]
    params = [toPersistValue gpuId, toPersistValue variant]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Int64, Vector (Int64, Double))
    convert [ PersistInt64 stepId
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] =
        return $ (stepId, VU.zip impls timings)
    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
        FROM
        (   SELECT id AS implId
                 , type
            FROM Implementation
            WHERE algorithmId IN
            (   SELECT algorithmId
                FROM Variant
                WHERE id = #{fromSqlKey variant}
            )
        )
        ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    query = [i|
SELECT stepId
     , ImplVector.impls
     , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
       AS timings
FROM StepTimer
INNER JOIN IndexedImpls
ON IndexedImpls.implId = StepTimer.implId

LEFT JOIN ImplVector

WHERE gpuId = ? AND variantId = ?
GROUP BY stepId
ORDER BY stepId ASC|]
