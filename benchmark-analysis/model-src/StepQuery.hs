{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module StepQuery
    ( QueryMode(..)
    , StepInfo(..)
    , StepInfoConfig(..)
    , stepInfoQuery
    , sortStepTimings
    ) where

import Control.Monad.ST (runST)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import qualified Data.Vector.Algorithms.Insertion as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

data QueryMode = Train | Validate | All deriving (Show, Eq)

data StepInfoConfig = StepInfoConfig
    { stepInfoQueryMode :: QueryMode
    , stepInfoAlgorithm :: Key Algorithm
    , stepInfoPlatform :: Key Platform
    , stepInfoCommit :: CommitId
    , stepInfoGraphProps :: Set Text
    , stepInfoStepProps :: Set Text
    , stepInfoSeed :: Int64
    , stepInfoDatasets :: Set (Key Dataset)
    , stepInfoFilterIncomplete :: Bool
    , stepInfoGraphs :: Percentage
    , stepInfoVariants :: Percentage
    , stepInfoSteps :: Percentage
    , stepInfoTimestamp :: UTCTime
    } deriving (Show)

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector Double)
    , stepBestImpl :: {-# UNPACK #-} !Int64
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

stepInfoQuery :: StepInfoConfig -> Query StepInfo
stepInfoQuery StepInfoConfig
  { stepInfoAlgorithm
  , stepInfoPlatform
  , stepInfoQueryMode
  , stepInfoCommit
  , stepInfoGraphProps
  , stepInfoStepProps
  , stepInfoSeed
  , stepInfoDatasets
  , stepInfoGraphs
  , stepInfoVariants
  , stepInfoSteps
  , stepInfoFilterIncomplete
  , stepInfoTimestamp
  } = Query{..}
  where
    datasets :: Set Text
    datasets = S.map showSqlKey stepInfoDatasets

    queryName :: Text
    queryName = "stepInfoQuery"

    inExpression :: Set Text -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause t = "'" <> t <> "'"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m StepInfo
    convert [ PersistInt64 (toSqlKey -> stepVariantId)
            , PersistInt64 stepId
            , PersistInt64 stepBestImpl
            , PersistByteString (byteStringToVector -> stepTimings)
            , PersistByteString (byteStringToVector -> graphProps)
            , rawStepProps
            ]
            | Just stepProps <- maybeStepProps
            = return $ StepInfo{..}
      where
        maybeStepProps = case rawStepProps of
            PersistNull -> Just graphProps
            PersistByteString (byteStringToVector -> stepProps) ->
                Just $ graphProps <> stepProps
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlInt64, SqlBlob, SqlBlob, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ CTE
        { cteParams =
            [ toPersistValue $ S.size stepInfoDatasets == 0
            , toPersistValue stepInfoSeed
            , toPersistValue stepInfoGraphs
            , toPersistValue $ S.size stepInfoDatasets == 0
            ]
        , cteQuery = [i|
GraphCount(graphCount) AS (
    SELECT COUNT(*)
    FROM Graph
    WHERE ? OR datasetId IN #{inExpression datasets}
),

FilteredGraphs AS (
    SELECT Graph.*, random_sample(0, ?, graphCount, ?) OVER graphWin AS keepGraph
    FROM Graph, GraphCount
    WHERE ? OR datasetId IN #{inExpression datasets}
    WINDOW graphWin AS (ORDER BY id)
)|]
        }

      , CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoSeed
            , toPersistValue stepInfoVariants
            , toPersistValue stepInfoAlgorithm
            ]
        , cteQuery = [i|
VariantCount(variantCount) AS (
    SELECT COUNT(*)
    FROM Variant

    INNER JOIN FilteredGraphs
    ON Variant.graphId = FilteredGraphs.id

    WHERE Variant.algorithmId = ?
),

FilteredVariants AS (
    SELECT Variant.*, random_sample(1, ?, variantCount, ?) OVER variantWin AS keepVariant, keepGraph
    FROM Variant, VariantCount

    INNER JOIN FilteredGraphs
    ON Variant.graphId = FilteredGraphs.id

    WHERE Variant.algorithmId = ?

    WINDOW variantWin AS (ORDER BY Variant.id)
)|]
        }

      , [] `inCTE` [i|
IndexedGraphProps(graphId, idx, property, value, count) AS (
    SELECT graphId
         , ROW_NUMBER() OVER (graph ORDER BY property)
         , property
         , value
         , COUNT() OVER graph
    FROM GraphProp
    WHERE property IN #{inExpression stepInfoGraphProps}
    WINDOW graph AS (PARTITION BY graphId)
),

IndexedStepProps(variantId, stepId, idx, property, value, count) AS (
    SELECT variantId
         , stepId
         , ROW_NUMBER() OVER (variantStep ORDER BY property)
         , property
         , value
         , COUNT() OVER variantStep
    FROM StepPropValue
    WHERE property IN #{inExpression stepInfoStepProps}
    WINDOW variantStep AS (PARTITION BY variantId, stepId)
)|]

      , [toPersistValue stepInfoAlgorithm] `inCTE` [i|
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

      , [] `inCTE` [i|
StepCount(stepCount) AS (
    SELECT COUNT(*)
    FROM FilteredVariants, generate_series(0, FilteredVariants.maxStepId)
)
|]

      , CTE
        { cteParams =
            [ toPersistValue stepInfoSeed
            , toPersistValue stepInfoSteps
            , toPersistValue stepInfoTimestamp
            , toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoPlatform
            , toPersistValue stepInfoCommit
            , toPersistValue $ not stepInfoFilterIncomplete
            ]
        , cteQuery = [i|
StepTiming(graphId, variantId, stepId, implId, timings, keepGraph, keepVariant, keepStep) AS (
    SELECT FilteredVariants.graphId
         , FilteredVariants.id
         , StepTimer.stepId
         , min_key(Impls.implId, avgTime, maxTime, minTime)
           FILTER (WHERE Impls.type == 'Core')
         , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
           AS timings
         , keepGraph
         , keepVariant
         , random_sample(2, ?, stepCount, ?) OVER (ORDER BY FilteredVariants.id, stepId)
           AS keepStep
    FROM FilteredVariants, ImplVector, StepCount

    INNER JOIN Run
    ON Run.variantId = FilteredVariants.id
    AND Run.validated = 1
    AND Run.timestamp < ?

    INNER JOIN RunConfig
    ON RunConfig.id = Run.runConfigId
    AND RunConfig.algorithmId = ?
    AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?

    INNER JOIN StepTimer
    ON Run.id = StepTimer.runId

    INNER JOIN IndexedImpls AS Impls
    ON Impls.implId = Run.implId

    GROUP BY FilteredVariants.id, StepTimer.stepId
    HAVING ? OR COUNT(Run.id) = MAX(Impls.count)
)|]
        }]

    params :: [PersistValue]
    params =
      [ toPersistValue $ S.size stepInfoStepProps
      , toPersistValue (stepInfoQueryMode == All)
      , toPersistValue (stepInfoQueryMode == Train)
      ]

    queryText = [i|
SELECT StepTiming.variantId
     , StepTiming.stepId
     , StepTiming.implId
     , StepTiming.timings
     , GraphProps.props
     , StepProps.props
FROM StepTiming

INNER JOIN
(   SELECT graphId, double_vector(value, idx, count) AS props
    FROM IndexedGraphProps
    GROUP BY graphId
) AS GraphProps
ON GraphProps.graphId = StepTiming.graphId

LEFT JOIN
(   SELECT variantId, stepId, double_vector(value, idx, count) AS props
    FROM IndexedStepProps
    GROUP BY variantId, stepId
) AS StepProps
ON StepTiming.variantId = StepProps.variantId
AND StepTiming.stepId = StepProps.stepId

WHERE (StepProps.props NOT NULL OR ? = 0)
AND ? OR (? = (keepGraph AND keepVariant AND keepStep))
ORDER BY StepTiming.variantId, StepTiming.stepId ASC|]
