{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Time (timePlotQuery) where

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
    -> Query ((Key Graph, Text), (Vector ImplTiming, Vector ImplTiming))
timePlotQuery algoId platformId commitId variants =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "timePlotQuery"

    inExpression :: Set (Key Variant) -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause = showText . fromSqlKey

    converter
        :: MonadConvert m
        => [PersistValue]
        -> m ((Key Graph, Text), (Vector ImplTiming, Vector ImplTiming))
    converter [ PersistInt64 (toSqlKey -> graphId)
              , PersistText graphName
              , PersistByteString (byteStringToVector -> implTimings)
              , externalTimings
              ]
              | Just extImplTimings <- maybeExternalTimings
              = return ((graphId, graphName), (implTimings, extImplTimings))
      where
        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case externalTimings of
            PersistNull -> Just VS.empty
            PersistByteString (byteStringToVector -> timings) -> Just timings
            _ -> Nothing

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlString, SqlBlob, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ [toPersistValue algoId] `inCTE` [i|
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
)|]

      , [toPersistValue algoId] `inCTE` [i|
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
)|]

      , CTE
        { cteParams =
            [ toPersistValue algoId
            , toPersistValue platformId
            , toPersistValue commitId
            ]
        , cteQuery = [i|
VariantTiming(graphId, graphName, variantConfigId, variantId, timings) AS (
    SELECT Graph.id
         , Graph.name
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
)|]
        }

      , [toPersistValue platformId] `inCTE` [i|
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
SELECT VariantTiming.graphId
     , VariantTiming.graphName
     , VariantTiming.timings
     , ExternalTiming.timings
FROM VariantTiming

INNER JOIN VariantConfig
ON VariantTiming.variantConfigId = VariantConfig.id

LEFT JOIN ExternalTiming
ON VariantTiming.variantId = ExternalTiming.variantId

WHERE VariantConfig.isDefault = TRUE
ORDER BY VariantTiming.variantId ASC|]
