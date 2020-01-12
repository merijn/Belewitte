{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module VariantQuery (VariantInfo(..), variantInfoQuery) where

import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

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
            , PersistByteString (byteStringToVector -> variantTimings)
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

        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case externalTimings of
            PersistNull -> Just VS.empty
            PersistByteString (byteStringToVector -> extImpls) -> Just extImpls
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlReal, SqlReal, SqlBlob, SqlBlob ]

    cteParams :: [PersistValue]
    cteParams =
      [ toPersistValue algoId
      , toPersistValue algoId
      , toPersistValue algoId
      , toPersistValue platformId
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
    ORDER BY id
),

IndexedExternalImpls(idx, implId, count) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY implId)
            , implId
            , COUNT() OVER ()
        FROM (SELECT id AS implId
                FROM ExternalImpl
                WHERE algorithmId = ?)
        ORDER BY implId
),

VariantTiming(runConfigId, variantId, bestNonSwitching, timings) AS (
    SELECT RunConfig.id
         , Variant.id
         , MIN(avgTime) FILTER (WHERE type == 'Core') AS bestNonSwitching
         , key_value_vector(Impls.count, Impls.idx, Impls.implId, avgTime)
           AS timings
    FROM RunConfig

    INNER JOIN Graph
    ON Graph.datasetId = RunConfig.datasetId

    INNER JOIN Variant
    ON Variant.graphId = Graph.id

    JOIN IndexedImpls AS Impls

    LEFT JOIN
    ( SELECT Run.runConfigId, Run.implId, Run.variantId, avgTime
      FROM Run

      INNER JOIN TotalTimer
      ON Run.id = TotalTimer.runId

      WHERE TotalTimer.name = 'computation'
    ) AS Timings
    ON RunConfig.id = Timings.runConfigId
    AND Variant.id = Timings.variantId
    AND Impls.implId = Timings.implId

    WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?

    GROUP BY RunConfig.id, Variant.id
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
SELECT VariantTiming.variantId
     , OptimalStep.optimal
     , VariantTiming.bestNonSwitching
     , VariantTiming.timings
     , ExternalTiming.timings
FROM VariantTiming

LEFT JOIN ExternalTiming
ON VariantTiming.variantId = ExternalTiming.variantId

INNER JOIN
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
ON VariantTiming.runConfigId = OptimalStep.runConfigId
AND VariantTiming.variantId = OptimalStep.variantId

ORDER BY VariantTiming.runConfigId, VariantTiming.variantId ASC|]
