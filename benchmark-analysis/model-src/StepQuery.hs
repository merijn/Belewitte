{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module StepQuery (StepInfo(..), stepInfoQuery) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.String.Interpolate.IsString (i)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU

import Core
import Query
import Schema
import Utils (byteStringToVector)

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector Double)
    , stepBestImpl :: {-# UNPACK #-} !Int64
    , stepVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , stepTimings :: {-# UNPACK #-} !(Vector (Int64, Double))
    } deriving (Show)

stepInfoQuery
    :: Key Algorithm -> Key Platform -> Set Text -> Set Text -> Query StepInfo
stepInfoQuery algoId platformId graphProperties stepProperties = Query{..}
  where
    isExplain :: Bool
    isExplain = False

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
            , PersistByteString (byteStringToVector -> rawStepProps)
            ] = return $ StepInfo{..}
      where
        stepProps = graphProps <> rawStepProps
        stepTimings = VU.zip impls timings

    convert l = logThrowM . Error . T.pack $ "Unexpected value: " ++ show l

    cteParams :: [PersistValue]
    cteParams = [ toPersistValue algoId , toPersistValue platformId ]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
IndexedGraphProps(graphId, idx, property, value) AS (
    SELECT graphId
         , ROW_NUMBER() OVER (PARTITION BY graphId ORDER BY property)
         , property
         , value
    FROM GraphProp
    WHERE property IN #{inExpression graphProperties}
),

IndexedStepProps(variantId, stepId, idx, property, value) AS (
    SELECT variantId
         , stepId
         , ROW_NUMBER() OVER (PARTITION BY variantId, stepId ORDER BY property)
         , property
         , value
    FROM StepProp
    WHERE property IN #{inExpression stepProperties}
),

IndexedImpls(idx, implId, type) AS (
    SELECT ROW_NUMBER() OVER (ORDER BY id)
         , id
         , type
    FROM Implementation
    WHERE algorithmId = ?
),

ImplVector(impls) AS (
    SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
    FROM IndexedImpls
),

Step(variantId, stepId, implId, minTime, timings) AS (
    SELECT variantId, stepId, IndexedImpls.implId
         , MIN(CASE IndexedImpls.type
               WHEN "Core" THEN avgTime
               ELSE NULL END)
         , double_vector(avgTime, idx, (SELECT COUNT(*) FROM IndexedImpls))
           AS timings
    FROM StepTimer

    INNER JOIN IndexedImpls
    ON StepTimer.implId = IndexedImpls.implId
    WHERE platformId = ?
    GROUP BY variantId, stepId
)|]]

    params :: [PersistValue]
    params = [ toPersistValue $ S.size graphProperties
             , toPersistValue $ S.size stepProperties
             , toPersistValue algoId
             ]

    queryText = [i|
SELECT Variant.id
     , Step.stepId
     , Step.implId
     , ImplVector.impls
     , Step.timings
     , GraphProps.props
     , StepProps.props
FROM Variant

INNER JOIN Step
ON Variant.id = Step.variantId

INNER JOIN
(   SELECT graphId, double_vector(value, idx, ?) AS props
    FROM IndexedGraphProps
    GROUP BY graphId
) AS GraphProps
ON GraphProps.graphId = Variant.graphId

INNER JOIN
(   SELECT variantId, stepId
         , double_vector(value, idx, ?) AS props
    FROM IndexedStepProps
    GROUP BY variantId, stepId
) AS StepProps
ON Variant.id = StepProps.variantId AND Step.stepId = StepProps.stepId

LEFT JOIN ImplVector
WHERE Variant.algorithmId = ?
ORDER BY Variant.id, Step.stepId ASC|]
