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
    , stepInfoDatasets :: Maybe (Set (Key Dataset))
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
  } = Query{convert = Filter converter, ..}
  where
    datasets :: Set Text
    datasets = maybe mempty (S.map showSqlKey) stepInfoDatasets

    queryName :: Text
    queryName = "stepInfoQuery"

    inExpression :: Set Text -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause t = "'" <> t <> "'"

    converter :: MonadConvert m => [PersistValue] -> m (Maybe StepInfo)
    converter [ PersistInt64 (toSqlKey -> stepVariantId)
              , PersistInt64 stepId
              , PersistInt64 stepBestImpl
              , PersistByteString (byteStringToVector -> stepTimings)
              , PersistByteString (byteStringToVector -> graphProps)
              , rawStepProps
              , PersistInt64 ((/=0) -> keepRow)
              ]
              | Just stepProps <- maybeStepProps
              = case stepInfoQueryMode of
                  All -> return $ Just StepInfo{..}
                  Train | keepRow -> return $ Just StepInfo{..}
                  Validate | not keepRow -> return $ Just StepInfo{..}
                  _ -> return Nothing
      where
        maybeStepProps = case rawStepProps of
            PersistNull -> Just graphProps
            PersistByteString (byteStringToVector -> stepProps) ->
                Just $ graphProps <> stepProps
            _ -> Nothing

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlInt64, SqlBlob, SqlBlob, SqlBlob, SqlBool ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ CTE
        { cteParams =
            [ toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoTimestamp
            , toPersistValue stepInfoTimestamp
            , toPersistValue $ maybe True S.null stepInfoDatasets
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
            [ toPersistValue stepInfoSeed
            , toPersistValue stepInfoGraphs
            , toPersistValue stepInfoSeed
            , toPersistValue stepInfoVariants
            ]
        , cteQuery = [i|
Variants AS (
    SELECT graphId
          , variantId
          , SUM(maxStepId + 1) OVER () AS stepCount
          , random_sample(0, ?, graphCount, ?)
            OVER (ORDER BY graphId)
            AS keepGraph
          , random_sample(1, ?, variantCount, ?)
            OVER (ORDER BY variantId)
            AS keepVariant
    FROM GraphVariants
    ORDER BY variantId
)|]
        }

      , [] `inCTE` [i|
GraphPropIndices AS (
    SELECT id
         , ROW_NUMBER() OVER (ORDER BY property) AS idx
         , COUNT() OVER () AS count
    FROM PropertyName
    WHERE property IN #{inExpression stepInfoGraphProps}
    AND NOT isStepProp
),
GraphProps AS (
    SELECT graphId, double_vector(value, idx, count) AS props
    FROM GraphPropValue
    INNER JOIN GraphPropIndices
    ON GraphPropValue.propId = GraphPropIndices.id
    GROUP BY graphId
)|]

      , [toPersistValue stepInfoAlgorithm] `inCTE` [i|
StepPropIndices AS (
    SELECT id
         , ROW_NUMBER() OVER stepProps AS idx
         , COUNT() OVER counts AS count
    FROM PropertyName
    INNER JOIN StepProp
    ON PropertyName.id = StepProp.propId

    WHERE algorithmId = ? AND property IN #{inExpression stepInfoStepProps}
    WINDOW stepProps AS (ORDER BY property)
         , counts AS
           (stepProps ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)
),
StepProps AS (
    SELECT variantId, stepId, double_vector(value, idx, count) AS props
    FROM StepPropValue
    INNER JOIN StepPropIndices
    ON StepPropValue.propId = StepPropIndices.id
    GROUP BY variantId, stepId
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
      ]

    params :: [PersistValue]
    params =
      [ toPersistValue stepInfoSeed
      , toPersistValue stepInfoSteps
      , toPersistValue stepInfoTimestamp
      , toPersistValue stepInfoAlgorithm
      , toPersistValue stepInfoAlgorithm
      , toPersistValue stepInfoPlatform
      , toPersistValue stepInfoCommit
      , toPersistValue $ S.size stepInfoStepProps
      , toPersistValue $ not stepInfoFilterIncomplete
      ]

    queryText = [i|
SELECT StepTimer.variantId
     , StepTimer.stepId
     , min_key(Impls.implId, avgTime, maxTime, minTime)
       FILTER (WHERE Impls.type == 'Core')
       AS minVal
     , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
       AS timings
     , GraphProps.props
     , StepProps.props
     , random_sample(2, ?, Variants.stepCount, ?) OVER
            (ORDER BY StepTimer.variantId, StepTimer.stepId)
       AND Variants.keepGraph AND Variants.keepVariant
FROM StepTimer, ImplVector
INNER JOIN Run
ON Run.id = StepTimer.runId

INNER JOIN IndexedImpls AS Impls
ON Impls.implId = Run.implId

INNER JOIN RunConfig
ON Run.runConfigId = RunConfig.id

INNER JOIN Variants
ON StepTimer.variantId = Variants.variantId

INNER JOIN GraphProps
ON GraphProps.graphId = Variants.graphId

LEFT JOIN StepProps
ON StepProps.variantId = StepTimer.variantId
AND StepProps.stepId = StepTimer.stepId

WHERE Run.validated = 1
AND Run.timestamp < ?
AND Run.algorithmId = ?
AND RunConfig.algorithmId = ?
AND RunConfig.platformId = ?
AND RunConfig.algorithmVersion = ?
AND (StepProps.props NOT NULL OR ? = 0)

GROUP BY StepTimer.variantId, StepTimer.stepId
HAVING ? OR COUNT(Run.id) = MAX(Impls.count)
ORDER BY StepTimer.variantId, StepTimer.stepId ASC|]
