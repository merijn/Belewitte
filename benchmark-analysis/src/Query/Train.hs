{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Train
    ( QueryMode(..)
    , StepInfo(..)
    , StepInfoConfig(..)
    , TrainStepConfig(..)
    , sortStepTimings
    , trainStepQuery
    ) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS

import Core
import Query
import Query.Step (StepInfo(..), StepInfoConfig(..), sortStepTimings)
import Schema
import Utils.PropValue
import Utils.Vector (byteStringToVector)

data QueryMode = Train | Validate | All deriving (Show, Eq)

data TrainStepConfig = TrainStepConfig
    { trainStepInfoConfig :: StepInfoConfig
    , trainStepQueryMode :: QueryMode
    , trainStepDatasets :: Maybe (Set (Key Dataset))
    , trainStepProps :: Set (Key PropertyName)
    , trainStepSeed :: Int64
    , trainStepGraphs :: Percentage
    , trainStepVariants :: Percentage
    , trainStepSteps :: Percentage
    } deriving (Show)

trainStepQuery :: TrainStepConfig -> Query StepInfo
trainStepQuery TrainStepConfig
  { trainStepInfoConfig = StepInfoConfig
    { stepInfoAlgorithm
    , stepInfoPlatform
    , stepInfoCommit
    , stepInfoFilterIncomplete
    , stepInfoTimestamp
    , stepInfoAllowNewer
    }
  , trainStepQueryMode
  , trainStepDatasets
  , trainStepProps
  , trainStepSeed
  , trainStepGraphs
  , trainStepVariants
  , trainStepSteps
  } = Query{convert = Filter converter, ..}
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
        NewerResults -> False
        NewerImpls -> True
        AllNewer -> True

    datasets :: Set Text
    datasets = maybe mempty (S.map showSqlKey) trainStepDatasets

    queryName :: Text
    queryName = "trainStepQuery"

    inExpression :: Set Text -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause t = "'" <> t <> "'"

    checkProperty :: PropValue -> Bool
    checkProperty PropValue{propValuePropId} =
        toSqlKey propValuePropId `S.member` trainStepProps

    converter :: MonadConvert m => [PersistValue] -> m (Maybe StepInfo)
    converter [ PersistInt64 (toSqlKey -> stepVariantId)
              , PersistInt64 stepId
              , PersistInt64 stepBestImpl
              , PersistInt64 stepBestIdx
              , PersistByteString (byteStringToVector -> stepTimings)
              , PersistByteString (byteStringToVector -> rawStepProps)
              , PersistInt64 ((/=0) -> keepRow)
              ]
              = case trainStepQueryMode of
                  All -> return $ Just StepInfo{..}
                  Train | keepRow -> return $ Just StepInfo{..}
                  Validate | not keepRow -> return $ Just StepInfo{..}
                  _ -> return Nothing
      where
        stepProps = VS.filter checkProperty rawStepProps

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlInt64, SqlInt64, SqlBlob, SqlBlob, SqlBlob, SqlBool ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoTimestamp
            , toPersistValue stepInfoTimestamp
            , toPersistValue $ maybe True S.null trainStepDatasets
            ]
        , cteQuery = [i|
GraphVariants AS (
    SELECT Graph.id AS graphId
         , Variant.id AS variantId
         , Variant.maxStepId
         , count_transitions(Graph.id) OVER graphTransitions AS graphCount
         , COUNT() OVER graphTransitions AS variantCount
    FROM Graph

    INNER JOIN Variant
    ON Variant.graphId = Graph.id

    INNER JOIN VariantConfig
    ON Variant.variantConfigId = VariantConfig.id

    WHERE Variant.algorithmId = ?
    AND VariantConfig.timestamp <= ?
    AND Graph.timestamp <= ?
    AND (? OR Graph.datasetId IN #{inExpression datasets})

    WINDOW graphTransitions AS
    (ORDER BY graphId ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)
)|]
        }

      , CTE
        { cteParams =
            [ toPersistValue trainStepSeed
            , toPersistValue trainStepGraphs
            , toPersistValue trainStepSeed
            , toPersistValue trainStepVariants
            ]
        , cteQuery = [i|
Variants(graphId, variantId, stepCount, keepGraph, keepVariant) AS (
    SELECT graphId
         , variantId
         , SUM(maxStepId + 1) OVER ()
         , random_sample(0, ?, graphCount, ?)
           OVER (ORDER BY graphId)
         , random_sample(1, ?, variantCount, ?)
           OVER (ORDER BY variantId)
    FROM GraphVariants
    ORDER BY variantId
)|]
        }

      , [toPersistValue stepInfoAlgorithm] `inCTE` [i|
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
GraphPropVectors(variantId, graphProps) AS (
    SELECT variantId
         , update_key_value_vector(emptyProps, idx, propId, value)
    FROM Variants, EmptyPropVector
    INNER JOIN GraphPropValue USING (graphId)
    INNER JOIN PropIndices USING (propId)
    GROUP BY variantId
)|]

      , [] `inCTE` [i|
StepPropVectors(variantId, stepId, stepProps) AS (
    SELECT variantId
         , ifnull(stepId, 0)
         , update_key_value_vector(graphProps, idx, propId, value)
    FROM GraphPropVectors
    LEFT JOIN StepPropValue USING (variantId)
    LEFT JOIN PropIndices USING (propId)
    GROUP BY variantId, stepId
)|]

      , CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue newerImpls
            , toPersistValue stepInfoTimestamp
            ]
        , cteQuery = [i|
IndexedImpls(idx, implId, type, timestamp, count) AS (
    SELECT ROW_NUMBER() OVER ()
         , id
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
      [ toPersistValue trainStepSeed
      , toPersistValue trainStepSteps
      , toPersistValue newerResults
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
SELECT StepTimer.variantId
     , StepTimer.stepId
     , min_key(Impls.implId, avgTime, maxTime, minTime)
       FILTER (WHERE Impls.type == 'Core')
     , min_key(Impls.idx, avgTime, maxTime, minTime, Impls.implId)
       FILTER (WHERE Impls.type == 'Core') - 1
     , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
     , stepProps
     , random_sample(2, ?, Variants.stepCount, ?) OVER
            (ORDER BY StepTimer.variantId, StepTimer.stepId)
       AND Variants.keepGraph AND Variants.keepVariant
FROM StepPropVectors, ImplVector
INNER JOIN StepTimer USING (variantId, stepId)
INNER JOIN Variants USING (variantId)

INNER JOIN Run
ON Run.id = StepTimer.runId

INNER JOIN RunConfig
ON Run.runConfigId = RunConfig.id

INNER JOIN IndexedImpls AS Impls USING (implId)

WHERE Run.validated = 1
AND ((? OR Run.timestamp < ?) OR (? AND Impls.timestamp > ?))
AND Run.algorithmId = ?
AND RunConfig.algorithmId = ?
AND RunConfig.platformId = ?
AND RunConfig.algorithmVersion = ?

GROUP BY StepTimer.variantId, StepTimer.stepId
HAVING ? OR COUNT(Run.id) = MAX(Impls.count)
ORDER BY StepTimer.variantId, StepTimer.stepId ASC|]
