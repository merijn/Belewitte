{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module VariantQuery
    ( VariantInfo(..)
    , variantInfoQuery
    , sortVariantTimings
    ) where

import Control.Monad.ST (runST)
import Data.Ord (comparing)
import Data.String.Interpolate.IsString (i)
import qualified Data.Vector.Algorithms.Insertion as V
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

sortVariantTimings :: VariantInfo -> VariantInfo
sortVariantTimings info@VariantInfo{..} = info
    { variantTimings = sortVector variantTimings
    , variantExternalTimings = sortVector variantExternalTimings
    }
  where
    sortVector :: Vector ImplTiming -> Vector ImplTiming
    sortVector vec = runST $ do
        mvec <- VS.thaw vec
        V.sortBy (comparing implTimingImpl) mvec
        VS.unsafeFreeze mvec

variantInfoQuery :: Key Algorithm -> Key Platform -> Query VariantInfo
variantInfoQuery algoId platformId = Query{..}
  where
    queryName :: Text
    queryName = "variantInfoQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    convert [ PersistInt64 (toSqlKey -> variantId)
            , PersistDouble variantOptimal
            , PersistDouble variantBestNonSwitching
            , PersistByteString (byteStringToVector -> variantTimings)
            , externalTimings
            ]
            | Just variantExternalTimings <- maybeExternalTimings
            = return VariantInfo{..}
      where
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
    SELECT ROW_NUMBER() OVER ()
         , id
         , type
         , COUNT() OVER ()
    FROM Implementation
    WHERE algorithmId = ?
),

IndexedExternalImpls(idx, implId, count) AS (
    SELECT ROW_NUMBER() OVER ()
         , id
         , COUNT() OVER ()
    FROM ExternalImpl
    WHERE algorithmId = ?
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
      ON Run.id = TotalTimer.runId AND TotalTimer.name = 'computation'
    ) AS Timings
    ON RunConfig.id = Timings.runConfigId
    AND Variant.id = Timings.variantId
    AND Impls.implId = Timings.implId

    WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?

    GROUP BY RunConfig.id, Variant.id
),

OptimalStep(runConfigId, variantId, optimal) AS (
    SELECT runConfigId, variantId, SUM(Step.minTime) AS optimal
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
),

ExternalTiming(variantId, timings) AS (
   SELECT Variant.id, key_value_vector(count, idx, Impls.implId, avgTime)
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

INNER JOIN OptimalStep
ON VariantTiming.runConfigId = OptimalStep.runConfigId
AND VariantTiming.variantId = OptimalStep.variantId

LEFT JOIN ExternalTiming
ON VariantTiming.variantId = ExternalTiming.variantId

ORDER BY VariantTiming.runConfigId, VariantTiming.variantId ASC|]
