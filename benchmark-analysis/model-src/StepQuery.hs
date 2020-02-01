{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module StepQuery (StepInfo(..), stepInfoQuery, sortStepTimings) where

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

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ [] `inCTE` [i|
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
)|]
      , [toPersistValue algoId] `inCTE` [i|
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
      , CTE
        { cteParams =
            [ toPersistValue ts
            , toPersistValue algoId
            , toPersistValue platformId
            , toPersistValue commitId
            ]
        , cteQuery = [i|
StepTiming(graphId, variantId, stepId, implId, timings) AS (
    SELECT Variant.graphId
         , Variant.id
         , StepTimer.stepId
         , min_key(Impls.implId, avgTime, maxTime, minTime)
           FILTER (WHERE Impls.type == 'Core')
         , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
           AS timings
    FROM Variant, ImplVector

    INNER JOIN Run
    ON Run.variantId = Variant.id
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

    GROUP BY Variant.id, StepTimer.stepId
)|]
        }]

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
