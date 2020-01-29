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

ImplVector(implTiming) AS (
    SELECT init_key_value_vector(implId, idx, count)
    FROM IndexedImpls
),

IndexedExternalImpls(idx, implId, count) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY id)
            , id
            , COUNT() OVER ()
    FROM ExternalImpl
    WHERE algorithmId = ?
),

ExternalImplVector(implTiming) AS (
    SELECT init_key_value_vector(implId, idx, count)
    FROM IndexedExternalImpls
),

VariantTiming(graphName, variantConfigId, variantId, timings) AS (
    SELECT Graph.name
         , Variant.variantConfigId
         , Variant.id
         , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
    FROM RunConfig, ImplVector

    INNER JOIN Run
    ON Run.runConfigId = RunConfig.id
    AND Run.validated
    AND Run.variantId IN #{inExpression variants}

    INNER JOIN TotalTimer
    ON Run.id = TotalTimer.runId
    AND TotalTimer.name = 'computation'

    INNER JOIN Variant
    ON Variant.id = Run.variantId

    INNER JOIN Graph
    ON Graph.id = Variant.graphId

    INNER JOIN IndexedImpls AS Impls
    ON Impls.implId = Run.implId

    WHERE RunConfig.algorithmId = ?
    AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?

    GROUP BY Variant.id
),

ExternalTiming(variantId, timings) AS (
   SELECT Variant.id
        , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
    FROM Variant, ExternalImplVector

    INNER JOIN ExternalTimer
    ON ExternalTimer.variantId = Variant.id
    AND ExternalTimer.name = 'computation'
    AND ExternalTimer.platformId = ?

    INNER JOIN IndexedExternalImpls AS Impls
    ON Impls.implId = ExternalTimer.implId

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
    commonTableExpressions = [[i|
IndexedImpls(algorithmId, idx, implId, count) AS (
    SELECT algorithmId
         , ROW_NUMBER() OVER (algorithm ORDER BY id) AS idx
         , id AS implId
         , COUNT() OVER (algorithm) AS count
    FROM Implementation
    WINDOW algorithm AS (PARTITION BY algorithmId)
),

ImplVector(algorithmId, implTiming) AS (
    SELECT algorithmId, init_key_value_vector(implId, idx, count)
    FROM IndexedImpls
    GROUP BY algorithmId
)|]]

    params :: [PersistValue]
    params =
      [ toPersistValue variant
      , toPersistValue commitId
      , toPersistValue platformId
      ]

    queryText = [i|
SELECT StepTimer.stepId
     , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
FROM Variant

INNER JOIN ImplVector
ON ImplVector.algorithmId = Variant.algorithmId

INNER JOIN Run
ON Run.variantId = Variant.id
AND Run.validated

INNER JOIN StepTimer
ON StepTimer.runId = Run.id

INNER JOIN RunConfig
ON RunConfig.id = Run.runConfigId
AND RunConfig.algorithmId = Variant.algorithmId

INNER JOIN IndexedImpls AS Impls
ON Impls.algorithmId = RunConfig.algorithmId
AND Impls.implId = Run.implId

WHERE Variant.id = ?
AND RunConfig.algorithmVersion = ?
AND RunConfig.platformId = ?

GROUP BY StepTimer.stepId
ORDER BY StepTimer.stepId ASC|]
