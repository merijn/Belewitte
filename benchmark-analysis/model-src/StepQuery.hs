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
    -> CommitId
    -> Set Text
    -> Set Text
    -> UTCTime
    -> Query StepInfo
stepInfoQuery algoId platformId commitId graphProperties stepProperties ts =
    Query{..}
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

    cteParams :: [PersistValue]
    cteParams =
      [ toPersistValue algoId
      , toPersistValue ts
      , toPersistValue algoId
      , toPersistValue platformId
      , toPersistValue commitId
      ]

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

StepTiming(runConfigId, graphId, variantId, stepId, implId, timings) AS (
    SELECT RunConfig.id
         , Variant.graphId
         , Variant.id
         , Step.value
         , min_key(Impls.implId, avgTime, maxTime, minTime)
         , key_value_vector(count, idx, Impls.implId, avgTime) AS timings
    FROM RunConfig

    INNER JOIN Graph
    ON RunConfig.datasetId = Graph.datasetId

    INNER JOIN Variant
    ON Graph.id = Variant.graphId

    JOIN generate_series(0, Variant.maxStepId) AS Step
    JOIN IndexedImpls AS Impls

    LEFT JOIN
    ( SELECT Run.runConfigId, Run.implId, Run.variantId, StepTimer.*
      FROM Run

      INNER JOIN StepTimer
      ON Run.id = StepTimer.runId

      WHERE Run.validated = 1 AND Run.timestamp < ?
    ) AS Timings
    ON RunConfig.id = Timings.runConfigId
    AND Variant.id = Timings.variantId
    AND Impls.implId = Timings.implId
    AND Step.value = Timings.stepId

    WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?

    GROUP BY RunConfig.id, Variant.id, Variant.graphId, Step.value
    HAVING timings NOT NULL
)
|]]

    params :: [PersistValue]
    params = [ toPersistValue $ S.size stepProperties ]

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

WHERE StepProps.props NOT NULL OR ? = 0
ORDER BY StepTiming.variantId, StepTiming.stepId ASC|]
