{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
import VectorUtils (byteStringToVector)

timePlotQuery
    :: Key Algorithm
    -> Key Platform
    -> Set (Key Variant)
    -> Query (Text, Vector (Int64, Double))
timePlotQuery algoId platformId variants = Query{..}
  where
    isExplain :: Bool
    isExplain = False

    inExpression :: Set (Key Variant) -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause = showText . fromSqlKey

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Text, Vector (Int64, Double))
    convert [ PersistText graph
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] = return $ (graph, VU.zip impls timings)
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlString, SqlBlob, SqlBlob ]

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
    params = [toPersistValue platformId, toPersistValue algoId]

    queryText = [i|
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
    WHERE platformId = ? AND name = "computation"
    GROUP BY variantId
    HAVING variantId IN #{inExpression variants}
) AS Total
ON Variant.id = Total.variantId

LEFT JOIN ImplVector

WHERE Variant.name = "default" AND Variant.algorithmId = ?
ORDER BY Variant.id ASC|]

levelTimePlotQuery
    :: Key Platform -> Key Variant -> Query (Int64, Vector (Int64, Double))
levelTimePlotQuery platformId variant = Query{..}
  where
    isExplain :: Bool
    isExplain = False

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
                WHERE id = ?
            )
        )
        ORDER BY implId
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
FROM StepTimer
INNER JOIN IndexedImpls
ON IndexedImpls.implId = StepTimer.implId

LEFT JOIN ImplVector

WHERE platformId = ? AND variantId = ?
GROUP BY stepId
ORDER BY stepId ASC|]
