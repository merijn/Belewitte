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

    params :: [PersistValue]
    params = [toPersistValue platformId, toPersistValue algoId]

    whereClauses :: Set Text -> Text
    whereClauses = T.intercalate " OR " . map clause . S.toAscList
      where
        clause t = [i|property = "#{t}"|]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m StepInfo
    convert [ PersistByteString (byteStringToVector -> graphProps)
            , PersistByteString (byteStringToVector -> rawStepProps)
            , PersistInt64 stepBestImpl
            , PersistInt64 (toSqlKey -> stepVariantId) , PersistInt64 stepId
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            ] = return $ StepInfo{..}
      where
        stepProps = graphProps <> rawStepProps
        stepTimings = VU.zip impls timings

    convert l = logThrowM . Error . T.pack $ "Unexpected value: " ++ show l

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedGraphProps(idx, property) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY property)
             , property
          FROM (SELECT DISTINCT property
                 FROM GraphProp
                WHERE #{whereClauses graphProperties})
         ORDER BY property
    ),

    IndexedStepProps(idx, property) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY property)
             , property
          FROM (SELECT DISTINCT property
                  FROM StepProp
                 WHERE #{whereClauses stepProperties})
         ORDER BY property
    ),

    IndexedImpls(idx, implId, type) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , type
          FROM (SELECT id AS implId
                     , type
                  FROM Implementation
                 WHERE algorithmId = #{fromSqlKey algoId})
         ORDER BY implId
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, (SELECT COUNT(*) FROM IndexedImpls))
          FROM IndexedImpls
    )|]]

    queryText = [i|
SELECT GraphProps.props
     , StepProps.props
     , Step.implId
     , Variant.id
     , Step.stepId
     , ImplVector.impls
     , Step.timings
FROM Variant
INNER JOIN Graph
ON Variant.graphId = Graph.id

INNER JOIN
(   SELECT variantId, stepId, IndexedImpls.implId
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

) AS Step
ON Variant.id = Step.variantId

INNER JOIN
(   SELECT graphId
         , double_vector(value, idx, #{S.size graphProperties}) AS props
    FROM GraphProp
    INNER JOIN IndexedGraphProps AS IdxProps
    ON IdxProps.property = GraphProp.property
    GROUP BY graphId
) AS GraphProps
ON GraphProps.graphId = Graph.id

INNER JOIN
(   SELECT variantId, stepId
         , double_vector(value, idx, #{S.size stepProperties}) AS props
    FROM StepProp
    INNER JOIN IndexedStepProps AS IdxProps
    ON IdxProps.property = StepProp.property
    GROUP BY variantId, stepId
) AS StepProps
ON Variant.id = StepProps.variantId AND Step.stepId = StepProps.stepId

LEFT JOIN ImplVector
WHERE Variant.algorithmId = ?
ORDER BY Variant.id, Step.stepId ASC|]
