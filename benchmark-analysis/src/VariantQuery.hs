{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module VariantQuery (VariantInfo(..), variantInfoQuery) where

import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import Schema
import Query
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    , variantExternalTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

variantInfoQuery :: Key Algorithm -> Key Platform -> Query VariantInfo
variantInfoQuery algoId platformId = Query{..}
  where
    queryName :: Text
    queryName = "variantInfoQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    convert [ PersistInt64 (toSqlKey -> variantId)
            , stepOptimal
            , PersistDouble variantBestNonSwitching
            , PersistByteString (byteStringToVector -> impls)
            , PersistByteString (byteStringToVector -> timings)
            , externalImpls
            , externalTimings
            ]

            | PersistDouble variantOptimal <- stepOptimal
            , Just variantExternalTimings <- maybeExternalTimings
            = return VariantInfo{..}

            | PersistNull <- stepOptimal
            , Just variantExternalTimings <- maybeExternalTimings
            = let variantOptimal = infinity in return VariantInfo{..}
      where
        !infinity = 1/0
        variantTimings = VS.zipWith ImplTiming impls timings

        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case (externalImpls, externalTimings) of
            (PersistNull, PersistNull) -> Just VS.empty
            (PersistNull, PersistByteString _) -> Nothing
            (PersistByteString extImpls, PersistNull) ->
                Just $ VS.map (\impl -> ImplTiming impl infinity)
                              (byteStringToVector extImpls)
            (PersistByteString extImpls, PersistByteString times) ->
                Just $ VS.zipWith ImplTiming (byteStringToVector extImpls)
                                             (byteStringToVector times)
            _ -> Nothing

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlReal, SqlReal, SqlBlob, SqlBlob, SqlBlob, SqlBlob]

    cteParams :: [PersistValue]
    cteParams = [toPersistValue algoId, toPersistValue algoId]

    commonTableExpressions :: [Text]
    commonTableExpressions = [[i|
    IndexedImpls(idx, implId, type, count) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY id)
             , id
             , type
             , COUNT() OVER ()
        FROM Implementation
        WHERE algorithmId = ?
        ORDER BY id
    ),

    ImplVector(impls) AS (
        SELECT int64_vector(implId, idx, count)
          FROM IndexedImpls
    ),

    IndexedExternalImpls(idx, implId, count) AS (
        SELECT ROW_NUMBER() OVER (ORDER BY implId)
             , implId
             , COUNT() OVER ()
          FROM (SELECT id AS implId
                  FROM ExternalImpl
                 WHERE algorithmId = ?)
         ORDER BY implId
    ),

    ExternalImplVector(impls) AS (
        SELECT int64_vector(implId, idx, count)
          FROM IndexedExternalImpls
    )|]]

    params :: [PersistValue]
    params = [ toPersistValue platformId, toPersistValue algoId
             , toPersistValue platformId
             ]

    queryText = [i|
SELECT OptimalStep.variantId
     , OptimalStep.optimal
     , Total.bestNonSwitching
     , ImplVector.impls
     , Total.timings
     , ExternalImplVector.impls
     , External.timings
FROM RunConfig

LEFT JOIN
(   SELECT runConfigId
         , variantId
         , SUM(Step.minTime) AS optimal
    FROM (
        SELECT Run.runConfigId
             , Run.variantId
             , stepId
             , MIN(avgTime) FILTER (WHERE type == 'Core') AS minTime
        FROM StepTimer

        INNER JOIN Run
        ON StepTimer.runId = Run.id

        INNER JOIN Implementation
        ON Run.implId = Implementation.id

        GROUP BY Run.runConfigId, Run.variantId, stepId
    ) AS Step
    GROUP BY runConfigId, variantId
) AS OptimalStep
ON RunConfig.id = OptimalStep.runConfigId

INNER JOIN
(   SELECT Run.variantId
         , MIN(avgTime) FILTER (WHERE type == 'Core') AS bestNonSwitching
         , double_vector(avgTime, idx, count) AS timings
      FROM TotalTimer

      INNER JOIN Run
      ON TotalTimer.runId = Run.id

      INNER JOIN IndexedImpls
      ON Run.implId = IndexedImpls.implId

      WHERE TotalTimer.name = 'computation'
      GROUP BY Run.variantId
) AS Total
ON OptimalStep.variantId = Total.variantId

LEFT JOIN
(   SELECT variantId
         , double_vector(avgTime, idx, count) AS timings
      FROM ExternalTimer
      INNER JOIN IndexedExternalImpls
      ON ExternalTimer.implId = IndexedExternalImpls.implId
      WHERE platformId = ? AND ExternalTimer.name = 'computation'
      GROUP BY variantId
) AS External
ON OptimalStep.variantId = External.variantId

LEFT JOIN ImplVector
LEFT JOIN ExternalImplVector

WHERE RunConfig.algorithmId = ? AND RunConfig.platformId = ?
ORDER BY OptimalStep.variantId ASC|]
