{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Step
    ( StepInfoConfig(..)
    , StepInfo(..)
    , algorithmVariantQuery
    , datasetVariantQuery
    , sortStepTimings
    , stepInfoQuery
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
import Utils.ImplTiming (ImplTiming(implTimingImpl))
import Utils.PropValue (PropValue)
import Utils.Vector (byteStringToVector)

data StepInfoConfig = StepInfoConfig
    { stepInfoAlgorithm :: Key Algorithm
    , stepInfoPlatform :: Key Platform
    , stepInfoCommit :: CommitId
    , stepInfoFilterIncomplete :: Bool
    , stepInfoTimestamp :: UTCTime
    , stepInfoAllowNewer :: AllowNewer
    } deriving (Show)

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector PropValue)
    , stepBestImpl :: {-# UNPACK #-} !Int64
    , stepBestIdx :: {-# UNPACK #-} !Int64
    , stepVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , stepTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

sortStepTimings :: StepInfo -> StepInfo
sortStepTimings info@StepInfo{..} =
    info { stepTimings = sortVector stepTimings }
  where
    sortVector :: Vector ImplTiming -> Vector ImplTiming
    sortVector vec = runST $ do
        mvec <- VS.thaw vec
        V.sortBy (comparing implTimingImpl) mvec
        VS.unsafeFreeze mvec

algorithmVariantQuery :: Key Algorithm -> Query (Key Variant)
algorithmVariantQuery algorithmId =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "algorithmVariantQuery"

    converter :: MonadConvert m => [PersistValue] -> m (Key Variant)
    converter [ PersistInt64 (toSqlKey -> variantId) ] = return variantId

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64 ]

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue algorithmId ]

    queryText = [i|
SELECT Variant.id
FROM Variant
WHERE Variant.algorithmId = ?
ORDER BY Variant.id ASC|]

datasetVariantQuery :: Key Algorithm -> Key Dataset -> Query (Key Variant)
datasetVariantQuery algorithmId datasetId =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "datasetVariantQuery"

    converter :: MonadConvert m => [PersistValue] -> m (Key Variant)
    converter [ PersistInt64 (toSqlKey -> variantId) ] = return variantId

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64 ]

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue datasetId, toPersistValue algorithmId ]

    queryText = [i|
SELECT Variant.id
FROM Variant
INNER JOIN Graph
ON Variant.graphId = Graph.id
WHERE Graph.datasetId = ? AND Variant.algorithmId = ?
ORDER BY Variant.id ASC|]

stepInfoQuery :: StepInfoConfig -> Key Variant -> Query StepInfo
stepInfoQuery StepInfoConfig{..} variantId =
    Query{convert = Simple converter, ..}
  where
    newerResults :: Bool
    newerResults = case stepInfoAllowNewer of
        NoNewer -> False
        NewerImpls -> False
        NewerResults -> True
        AllNewer -> True

    newerImpls :: Bool
    newerImpls = case stepInfoAllowNewer of
        NoNewer -> False
        NewerImpls -> True
        NewerResults -> False
        AllNewer -> True

    queryName :: Text
    queryName = "stepInfoQuery"

    converter :: MonadConvert m => [PersistValue] -> m StepInfo
    converter
        [ PersistInt64 (toSqlKey -> stepVariantId)
        , PersistInt64 stepId
        , PersistInt64 stepBestImpl
        , PersistInt64 stepBestIdx
        , PersistByteString (byteStringToVector -> stepTimings)
        , PersistByteString (byteStringToVector -> stepProps)
        ]
        = return StepInfo{..}

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlInt64, SqlInt64, SqlBlob, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue variantId
            , toPersistValue variantId
            ]
        , cteQuery = [i|
PropIndices AS (
    SELECT PropertyName.id AS propId
         , ROW_NUMBER() OVER idWindow AS idx
         , COUNT() OVER countWindow AS count
    FROM PropertyName
    LEFT JOIN StepProp
    ON PropertyName.id = StepProp.propId
    WHERE NOT isStepProp OR (StepProp.algorithmId = ?)
    WINDOW idWindow AS(ORDER BY PropertyName.id),
           countWindow AS
           (idWindow ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)
),
EmptyPropVector(emptyProps) AS (
    SELECT init_key_value_vector_nan(propId, idx, count)
    FROM PropIndices
),
GraphPropVector(variantId, graphProps) AS (
    SELECT Variant.id
         , update_key_value_vector(emptyProps, idx, propId, value)
    FROM Variant, EmptyPropVector
    INNER JOIN GraphPropValue USING (graphId)
    INNER JOIN PropIndices USING (propId)
    WHERE Variant.id = ?
    GROUP BY Variant.id
),
StepProps(variantId, stepId, stepProps) AS (
    SELECT variantId, ifnull(stepId, 0)
         , update_key_value_vector(graphProps, idx, propId, value)
    FROM GraphPropVector
    LEFT JOIN StepPropValue USING (variantId)
    LEFT JOIN PropIndices USING (propId)
    WHERE variantId = ?
    GROUP BY variantId, stepId
    ORDER BY variantId, stepId
)|]
        }

      , CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue newerImpls
            , toPersistValue stepInfoTimestamp
            ]
        , cteQuery = [i|
IndexedImpls(idx, implId, type, timestamp, count) AS (
    SELECT ROW_NUMBER() OVER ()
         , Implementation.id
         , type
         , timestamp
         , COUNT() OVER ()
    FROM Implementation
    WHERE algorithmId = ? AND (? OR Implementation.timestamp <= ?)
),

ImplVector(implTiming) AS (
    SELECT init_key_value_vector(implId, idx, count)
    FROM IndexedImpls
)|]
        }
      ]

    params :: [PersistValue]
    params =
        [ toPersistValue newerResults
        , toPersistValue stepInfoTimestamp
        , toPersistValue newerImpls
        , toPersistValue stepInfoTimestamp
        , toPersistValue stepInfoAlgorithm
        , toPersistValue stepInfoAlgorithm
        , toPersistValue stepInfoPlatform
        , toPersistValue stepInfoCommit
        , toPersistValue $ not stepInfoFilterIncomplete
        ]

    queryText = [i|
SELECT StepProps.variantId
     , StepProps.stepId
     , min_key(Impls.implId, avgTime, maxTime, minTime)
       FILTER (WHERE Impls.type == 'Core')
     , min_key(Impls.idx, avgTime, maxTime, minTime, Impls.implId)
       FILTER (WHERE Impls.type == 'Core') - 1
     , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
     , StepProps.stepProps
FROM StepProps, ImplVector

INNER JOIN StepTimer USING (variantId, stepId)

INNER JOIN Run
ON StepTimer.runId = Run.id

INNER JOIN RunConfig
ON RunConfig.id = Run.runConfigId

INNER JOIN IndexedImpls AS Impls USING (implId)

WHERE Run.validated = 1
AND ((? OR Run.timestamp < ?) OR (? AND Impls.timestamp > ?))
AND Run.algorithmId = ?
AND RunConfig.algorithmId = ?
AND RunConfig.platformId = ?
AND RunConfig.algorithmVersion = ?

GROUP BY StepProps.variantId, StepProps.stepId
HAVING ? OR COUNT(Run.id) = MAX(Impls.count)
ORDER BY StepProps.variantId, StepProps.stepId ASC|]
