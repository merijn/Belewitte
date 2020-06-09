{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Level (levelTimePlotQuery) where

import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

levelTimePlotQuery
    :: Key Platform
    -> CommitId
    -> Key Variant
    -> Query (Int64, Vector ImplTiming)
levelTimePlotQuery platformId commitId variant =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "levelTimePlotQuery"

    converter
        :: MonadConvert m => [PersistValue] -> m (Int64, Vector ImplTiming)
    converter [ PersistInt64 stepId
              , PersistByteString (byteStringToVector -> timings)
              ] = return (stepId, timings)

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions = [ [] `inCTE` [i|
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
