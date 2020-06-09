{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.StepHeatmap
    ( StepHeatmap(..)
    , StepHeatmapConfig(..)
    , stepHeatmapQuery
    ) where

import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

data StepHeatmapConfig = StepHeatmapConfig
    { stepHeatmapAlgorithm :: Key Algorithm
    , stepHeatmapPlatform :: Key Platform
    , stepHeatmapCommit :: CommitId
    , stepHeatmapVariant :: Key Variant
    } deriving (Show)

data StepHeatmap =
  StepHeatmap
    { stepHeatmapVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepHeatmapId :: {-# UNPACK #-} !Int64
    , stepHeatmapTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

stepHeatmapQuery :: StepHeatmapConfig -> Query StepHeatmap
stepHeatmapQuery StepHeatmapConfig
  { stepHeatmapAlgorithm
  , stepHeatmapPlatform
  , stepHeatmapCommit
  , stepHeatmapVariant
  } = Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "stepHeatmapQuery"

    converter :: MonadConvert m => [PersistValue] -> m StepHeatmap
    converter [ PersistInt64 (toSqlKey -> stepHeatmapVariantId)
              , PersistInt64 stepHeatmapId
              , PersistByteString (byteStringToVector -> stepHeatmapTimings)
              ] = return StepHeatmap{..}

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ [toPersistValue stepHeatmapAlgorithm] `inCTE` [i|
IndexedImpls(idx, implId, type, count) AS (
    SELECT ROW_NUMBER() OVER ()
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
      ]

    params :: [PersistValue]
    params =
      [ toPersistValue stepHeatmapVariant
      , toPersistValue stepHeatmapAlgorithm
      , toPersistValue stepHeatmapAlgorithm
      , toPersistValue stepHeatmapPlatform
      , toPersistValue stepHeatmapCommit
      ]

    queryText = [i|
SELECT StepTimer.variantId
     , StepTimer.stepId
     , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
       AS timings
FROM StepTimer, ImplVector

INNER JOIN Run
ON Run.id = StepTimer.runId

INNER JOIN IndexedImpls AS Impls
ON Impls.implId = Run.implId

INNER JOIN RunConfig
ON Run.runConfigId = RunConfig.id

INNER JOIN Variants
ON StepTimer.variantId = ?

WHERE Run.algorithmId = ?
AND RunConfig.algorithmId = ?
AND RunConfig.platformId = ?
AND RunConfig.algorithmVersion = ?

GROUP BY StepTimer.variantId, StepTimer.stepId
ORDER BY StepTimer.variantId, StepTimer.stepId ASC|]
