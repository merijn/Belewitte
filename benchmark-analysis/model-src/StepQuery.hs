{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module StepQuery (StepInfo(..), stepInfoQuery) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Query
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector Double)
    , stepBestImpl :: {-# UNPACK #-} !Int64
    , stepVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , stepTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

stepInfoQuery
    :: Key Algorithm
    -> Key Platform
    -> Set Text
    -> Set Text
    -> UTCTime
    -> Query StepInfo
stepInfoQuery algoId platformId graphProperties stepProperties ts = Query{..}
  where
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
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
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

        stepTimings = VS.zipWith ImplTiming impls timings

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlInt64, SqlInt64, SqlBlob, SqlBlob, SqlBlob, SqlBlob ]

    cteParams :: [PersistValue]
    cteParams = [ toPersistValue algoId , toPersistValue ts ]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
IndexedGraphProps(graphId, idx, property, value, count) AS (
    SELECT graphId
         , ROW_NUMBER() OVER (graph ORDER BY property)
         , property
         , value
         , COUNT() OVER graph
    FROM GraphProp
    WHERE property IN #{inExpression graphProperties}
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
    WHERE property IN #{inExpression stepProperties}
    WINDOW variantStep AS (PARTITION BY variantId, stepId)
),

IndexedImpls(idx, implId, type, count) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY id)
         , id
         , type
         , COUNT() OVER ()
    FROM Implementation
    WHERE algorithmId = ?
),

ImplVector(impls) AS (
    SELECT int64_vector(implId, idx, count)
    FROM IndexedImpls
),

Step(runConfigId, variantId, stepId, implId, minTime, timings) AS (
    SELECT Run.runConfigId, Run.variantId, stepId, IndexedImpls.implId
         , MIN(avgTime) FILTER (WHERE IndexedImpls.type == 'Core')
         , double_vector(avgTime, idx, count) AS timings
    FROM StepTimer

    INNER JOIN Run
    ON StepTimer.runId = Run.id

    INNER JOIN IndexedImpls
    ON Run.implId = IndexedImpls.implId

    WHERE Run.validated == 1 AND Run.timestamp < ?
    GROUP BY Run.runConfigId, Run.variantId, stepId
)|]]

    params :: [PersistValue]
    params = [ toPersistValue algoId
             , toPersistValue platformId
             , toPersistValue $ S.size stepProperties
             ]

    queryText = [i|
SELECT Step.variantId
     , Step.stepId
     , Step.implId
     , ImplVector.impls
     , Step.timings
     , GraphProps.props
     , StepProps.props
FROM RunConfig

INNER JOIN Step
ON RunConfig.id = Step.runConfigId

INNER JOIN Variant
ON Step.variantId = Variant.id

INNER JOIN
(   SELECT graphId, double_vector(value, idx, count) AS props
    FROM IndexedGraphProps
    GROUP BY graphId
) AS GraphProps
ON GraphProps.graphId = Variant.graphId

LEFT JOIN
(   SELECT variantId, stepId
         , double_vector(value, idx, count) AS props
    FROM IndexedStepProps
    GROUP BY variantId, stepId
) AS StepProps
ON Step.variantId = StepProps.variantId AND Step.stepId = StepProps.stepId

LEFT JOIN ImplVector
WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
  AND (StepProps.props NOT NULL OR ? = 0)
ORDER BY Step.variantId, Step.stepId ASC|]
