{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module PlotQuery (timePlotQuery, levelTimePlotQuery) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

timePlotQuery
    :: Key Algorithm
    -> Key Platform
    -> CommitId
    -> Set (Key Variant)
    -> Query (Text, (Vector ImplTiming, Vector ImplTiming))
timePlotQuery algoId platformId commitId variants = Query{..}
  where
    queryName :: Text
    queryName = "timePlotQuery"

    inExpression :: Set (Key Variant) -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause = showText . fromSqlKey

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue]
        -> m (Text, (Vector ImplTiming, Vector ImplTiming))
    convert [ PersistText graph
            , PersistByteString (byteStringToVector -> implTimings)
            , externalTimings
            ]
            | Just extImplTimings <- maybeExternalTimings
            = return $ (graph, (implTimings, extImplTimings))
      where
        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case externalTimings of
            PersistNull -> Just VS.empty
            PersistByteString (byteStringToVector -> timings) -> Just timings
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlString, SqlBlob, SqlBlob ]

    cteParams :: [PersistValue]
    cteParams =
      [ toPersistValue algoId
      , toPersistValue algoId
      , toPersistValue algoId
      , toPersistValue platformId
      , toPersistValue commitId
      , toPersistValue platformId
      ]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
IndexedImpls(idx, implId, type, count) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY id)
            , id
            , type
            , COUNT() OVER ()
    FROM Implementation
    WHERE algorithmId = ?
),

IndexedExternalImpls(idx, implId, count) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY id)
            , id
            , COUNT() OVER ()
    FROM ExternalImpl
    WHERE algorithmId = ?
),

VariantTiming(runConfigId, graphName, variantConfigId, variantId, timings) AS (
    SELECT RunConfig.id
         , Graph.name
         , Variant.variantConfigId
         , Variant.id
         , key_value_vector(count, idx, Impls.implId, avgTime) AS timings
    FROM RunConfig

    INNER JOIN Graph
    ON RunConfig.datasetId = Graph.datasetId

    INNER JOIN Variant
    ON Graph.id = Variant.graphId
    AND Variant.id IN #{inExpression variants}

    JOIN IndexedImpls AS Impls

    LEFT JOIN
    ( SELECT Run.runConfigId
           , Run.implId
           , Run.variantId
           , avgTime
      FROM Run

      INNER JOIN TotalTimer
      ON Run.id = TotalTimer.runId

      WHERE TotalTimer.name = 'computation'
      AND Run.variantId IN #{inExpression variants}
    ) AS Timings
    ON RunConfig.id = Timings.runConfigId
    AND Variant.id = Timings.variantId
    AND Impls.implId = Timings.implId

    WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?

    GROUP BY RunConfig.id, Variant.id
    HAVING timings NOT NULL
),

ExternalTiming(variantId, timings) AS (
   SELECT Variant.id
        , key_value_vector(count, idx, Impls.implId, avgTime)
    FROM Variant, IndexedExternalImpls AS Impls

    LEFT JOIN ExternalTimer
    ON Impls.implId = ExternalTimer.implId

    AND ExternalTimer.name = 'computation' AND platformId = ?
    GROUP BY variantId
)|]]

    params :: [PersistValue]
    params = []

    queryText = [i|
SELECT VariantTiming.graphName
     , VariantTiming.timings
     , ExternalTiming.timings
FROM VariantTiming

INNER JOIN VariantConfig
ON VariantTiming.variantConfigId = VariantConfig.id

LEFT JOIN ExternalTiming
ON VariantTiming.variantId = ExternalTiming.variantId

WHERE VariantConfig.isDefault = TRUE
ORDER BY VariantTiming.variantId ASC|]

levelTimePlotQuery
    :: Key Platform
    -> CommitId
    -> Key Variant
    -> Query (Int64, Vector ImplTiming)
levelTimePlotQuery platformId commitId variant = Query{..}
  where
    queryName :: Text
    queryName = "levelTimePlotQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Int64, Vector ImplTiming)
    convert [ PersistInt64 stepId
            , PersistByteString (byteStringToVector -> timings)
            ] = return $ (stepId, timings)

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlBlob ]

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params =
      [ toPersistValue variant
      , toPersistValue commitId
      , toPersistValue platformId
      ]

    queryText = [i|
SELECT Step.value
     , key_value_vector(count, idx, Impls.implId, avgTime) AS timings
FROM RunConfig

INNER JOIN Graph
ON RunConfig.datasetId = Graph.datasetId

INNER JOIN Variant
ON Graph.id = Variant.graphId

JOIN generate_series(0, Variant.maxStepId) AS Step

JOIN
( SELECT algorithmId
       , ROW_NUMBER() OVER (algorithm ORDER BY id) AS idx
       , id AS implId
       , COUNT() OVER (algorithm) AS count
  FROM Implementation
  WINDOW algorithm AS (PARTITION BY algorithmId)
) AS Impls
ON Variant.algorithmId = Impls.algorithmId

LEFT JOIN
( SELECT runConfigId, implId, variantId, stepId, avgTime
  FROM Run

  INNER JOIN StepTimer
  ON Run.id = StepTimer.runId
) AS Timings
ON RunConfig.id = Timings.runConfigId
AND Variant.id = Timings.variantId
AND Impls.implId = Timings.implId
AND Step.value = Timings.stepId

WHERE Variant.id = ? AND RunConfig.algorithmVersion = ?
GROUP BY RunConfig.id, Step.value
HAVING timings NOT NULL AND RunConfig.platformId = ?
ORDER BY RunConfig.id, Step.value ASC|]
