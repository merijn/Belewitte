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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU

import Core
import Query
import Schema
import Utils.Vector (byteStringToVector)

timePlotQuery
    :: Key Algorithm
    -> Key Platform
    -> Set (Key Variant)
    -> Query (Text, (Vector (Int64, Double), Vector (Int64, Double)))
timePlotQuery algoId platformId variants = Query{..}
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
        -> m (Text, (Vector (Int64, Double), Vector (Int64, Double)))
    convert [ PersistText graph
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            , externalImpls
            , externalTimings
            ]
            | Just extImplTimings <- maybeExternalTimings
            = return $ (graph, (implTimings, extImplTimings))
      where
        !infinity = 1/0
        implTimings = VU.zip impls timings

        maybeExternalTimings :: Maybe (Vector (Int64, Double))
        maybeExternalTimings = case (externalImpls, externalTimings) of
            (PersistNull, PersistNull) -> Just VU.empty
            (PersistNull, PersistByteString _) -> Nothing
            (PersistByteString extImpls, PersistNull) ->
                Just $ VU.map (, infinity) (byteStringToVector extImpls)
            (PersistByteString extImpls, PersistByteString times) ->
                Just $ VU.zip (byteStringToVector extImpls)
                              (byteStringToVector times)
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlString, SqlBlob, SqlBlob, SqlBlob, SqlBlob ]

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
        SELECT ROW_NUMBER() OVER (ORDER BY id)
             , id
        FROM ExternalImpl
        WHERE algorithmId = ?
        ORDER BY id
    ),

    ExternalImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedExternalImpls))
          FROM IndexedExternalImpls
    )|]]

    params :: [PersistValue]
    params = [ toPersistValue platformId, toPersistValue platformId
             , toPersistValue algoId ]

    queryText = [i|
SELECT Graph.name
     , ImplVector.impls
     , Total.timings
     , ExternalImplVector.impls
     , External.timings
FROM RunConfig

INNER JOIN
(   SELECT Run.runConfigId
         , Run.variantId
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
    FROM TotalTimer

    INNER JOIN Run
    ON Run.id = TotalTimer.runId

    INNER JOIN IndexedImpls
    ON IndexedImpls.implId = Run.implId

    WHERE name = 'computation'
    GROUP BY Run.runConfigId, Run.variantId
    HAVING Run.variantId IN #{inExpression variants}
) AS Total
ON RunConfig.id = Total.runConfigId

INNER JOIN Variant
ON Total.variantId = Variant.id

INNER JOIN VariantConfig
ON Variant.variantConfigId = VariantConfig.id

INNER JOIN Graph
ON Variant.graphId = Graph.id

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
ON Total.variantId = External.variantId

LEFT JOIN ImplVector
LEFT JOIN ExternalImplVector

WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
  AND VariantConfig.isDefault = TRUE
ORDER BY Total.variantId ASC|]

levelTimePlotQuery
    :: Key Platform -> Key Variant -> Query (Int64, Vector (Int64, Double))
levelTimePlotQuery platformId variant = Query{..}
  where
    queryName :: Text
    queryName = "levelTimePlotQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Int64, Vector (Int64, Double))
    convert [ PersistInt64 stepId
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] =
        return $ (stepId, VU.zip impls timings)
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlBlob, SqlBlob ]

    cteParams :: [PersistValue]
    cteParams = [toPersistValue variant]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY id)
             , id
             , type
        FROM Implementation
        WHERE algorithmId IN
            (   SELECT algorithmId
                FROM Variant
                WHERE id = ?
            )
        ORDER BY id
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    params :: [PersistValue]
    params = [toPersistValue platformId, toPersistValue variant]

    queryText = [i|
SELECT stepId
     , ImplVector.impls
     , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
       AS timings
FROM RunConfig

INNER JOIN Run
ON RunConfig.id = Run.runConfigId

INNER JOIN IndexedImpls
ON IndexedImpls.implId = Run.implId

INNER JOIN StepTimer
ON Run.id = StepTimer.runId

LEFT JOIN ImplVector

WHERE RunConfig.platformId = ? AND Run.variantId = ?
GROUP BY stepId
ORDER BY stepId ASC|]
