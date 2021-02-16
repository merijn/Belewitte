{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Variant
    ( VariantInfo(..)
    , VariantInfoConfig(..)
    , variantInfoQuery
    , sortVariantTimings
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

data VariantInfoConfig = VariantInfoConfig
    { variantInfoAlgorithm :: Key Algorithm
    , variantInfoPlatform :: Key Platform
    , variantInfoCommit :: CommitId
    , variantInfoVariantConfigs :: Maybe (Set (Key VariantConfig))
    , variantInfoDatasets :: Maybe (Set (Key Dataset))
    , variantInfoTimestamp :: UTCTime
    , variantInfoAllowNewer :: AllowNewer
    , variantInfoFilterIncomplete :: Bool
    } deriving (Show)

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    , variantExternalTimings :: {-# UNPACK #-} !(Vector ImplTiming)
    } deriving (Show)

sortVariantTimings :: VariantInfo -> VariantInfo
sortVariantTimings info@VariantInfo{..} = info
    { variantTimings = sortVector variantTimings
    , variantExternalTimings = sortVector variantExternalTimings
    }
  where
    sortVector :: Vector ImplTiming -> Vector ImplTiming
    sortVector vec = runST $ do
        mvec <- VS.thaw vec
        V.sortBy (comparing implTimingImpl) mvec
        VS.unsafeFreeze mvec

variantInfoQuery :: VariantInfoConfig -> Query VariantInfo
variantInfoQuery VariantInfoConfig
  { variantInfoAlgorithm
  , variantInfoPlatform
  , variantInfoCommit
  , variantInfoDatasets
  , variantInfoVariantConfigs
  , variantInfoTimestamp
  , variantInfoAllowNewer
  , variantInfoFilterIncomplete
  } = Query{convert = Simple converter, ..}
  where
    newerResults :: Bool
    newerResults = case variantInfoAllowNewer of
        NoNewer -> False
        NewerImpls -> False
        NewerResults -> True
        AllNewer -> True

    newerImpls :: Bool
    newerImpls = case variantInfoAllowNewer of
        NoNewer -> False
        NewerImpls -> True
        NewerResults -> False
        AllNewer -> True

    datasets :: Set Text
    datasets = maybe mempty (S.map showSqlKey) variantInfoDatasets

    variantConfigs :: Set Text
    variantConfigs = maybe mempty (S.map showSqlKey) variantInfoVariantConfigs

    inExpression :: Set Text -> Text
    inExpression s = "(" <> clauses <> ")"
      where
        clauses = T.intercalate ", " . map clause . S.toAscList $ s
        clause t = "'" <> t <> "'"

    queryName :: Text
    queryName = "variantInfoQuery"

    converter
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    converter [ PersistInt64 (toSqlKey -> variantId)
              , PersistDouble variantOptimal
              , PersistDouble variantBestNonSwitching
              , PersistByteString (byteStringToVector -> variantTimings)
              , externalTimings
              ]
              | Just variantExternalTimings <- maybeExternalTimings
              = return VariantInfo{..}
      where
        maybeExternalTimings :: Maybe (Vector ImplTiming)
        maybeExternalTimings = case externalTimings of
            PersistNull -> Just VS.empty
            PersistByteString (byteStringToVector -> extImpls) -> Just extImpls
            _ -> Nothing

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlReal, SqlReal, SqlBlob, SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ CTE
        { cteParams =
            [ toPersistValue variantInfoAlgorithm
            , toPersistValue newerImpls
            , toPersistValue variantInfoTimestamp
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

      , [toPersistValue variantInfoAlgorithm] `inCTE` [i|
IndexedExternalImpls(idx, implId, count) AS (
    SELECT ROW_NUMBER() OVER ()
         , id
         , COUNT() OVER ()
    FROM ExternalImpl
    WHERE algorithmId = ?
),

ExternalImplVector(implTiming) AS (
    SELECT init_key_value_vector(implId, idx, count)
    FROM IndexedExternalImpls
)|]

      , CTE
        { cteParams =
            [ toPersistValue newerResults
            , toPersistValue variantInfoTimestamp
            , toPersistValue newerImpls
            , toPersistValue variantInfoTimestamp
            , toPersistValue variantInfoAlgorithm
            , toPersistValue variantInfoPlatform
            , toPersistValue variantInfoCommit
            , toPersistValue $ maybe True S.null variantInfoDatasets
            , toPersistValue $ maybe True S.null variantInfoVariantConfigs
            , toPersistValue $ not variantInfoFilterIncomplete
            ]
        , cteQuery = [i|
VariantTiming(variantId, bestNonSwitching, timings) AS (
    SELECT Run.variantId
         , MIN(avgTime) FILTER (WHERE type == 'Core') AS bestNonSwitching
         , update_key_value_vector(implTiming, Impls.idx, Impls.implId, avgTime)
           AS timings
    FROM RunConfig, ImplVector

    INNER JOIN Run
    ON Run.runConfigId = RunConfig.id
    AND Run.validated AND
    ((? OR Run.timestamp < ?) OR (? AND Impls.timestamp > ?))

    INNER JOIN IndexedImpls AS Impls
    ON Impls.implId = Run.implId

    INNER JOIN TotalTimer
    ON Run.id = TotalTimer.runId AND TotalTimer.name = 'computation'

    INNER JOIN Variant
    ON Variant.id = Run.variantId

    WHERE RunConfig.algorithmId = ?
    AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?
    AND (? OR RunConfig.datasetId IN #{inExpression datasets})
    AND (? OR Variant.variantConfigId IN #{inExpression variantConfigs})

    GROUP BY Run.variantId
    HAVING ? OR COUNT(Run.id) = MAX(Impls.count)
)|]
        }

      , CTE
        { cteParams =
            [ toPersistValue variantInfoAlgorithm
            , toPersistValue variantInfoPlatform
            , toPersistValue variantInfoCommit
            , toPersistValue $ maybe True S.null variantInfoDatasets
            ]
        , cteQuery = [i|
OptimalStep(variantId, optimal) AS (
    SELECT variantId, SUM(Step.minTime) AS optimal
    FROM (
        SELECT Run.variantId, stepId
             , MIN(avgTime) FILTER (WHERE type == 'Core') AS minTime
        FROM StepTimer

        INNER JOIN Run
        ON StepTimer.runId = Run.id AND Run.validated

        INNER JOIN RunConfig
        ON Run.runConfigId = RunConfig.id
        AND RunConfig.algorithmId = ?
        AND RunConfig.platformId = ?
        AND RunConfig.algorithmVersion = ?
        AND (? OR RunConfig.datasetId IN #{inExpression datasets})

        INNER JOIN Implementation
        ON Run.implId = Implementation.id

        GROUP BY Run.variantId, stepId
    ) AS Step
    GROUP BY variantId
)|]
        }

      , CTE
        { cteParams =
            [ toPersistValue variantInfoPlatform
            , toPersistValue $ not variantInfoFilterIncomplete
            ]
        , cteQuery = [i|
ExternalTiming(variantId, timings) AS (
   SELECT Variant.id
        , update_key_value_vector(implTiming, idx, Impls.implId, avgTime)
   FROM Variant, ExternalImplVector

   INNER JOIN ExternalTimer
   ON ExternalTimer.variantId = Variant.id
   AND ExternalTimer.name = 'computation'
   AND ExternalTimer.platformId = ?

   INNER JOIN IndexedExternalImpls AS Impls
   ON Impls.implId = ExternalTimer.implId

   GROUP BY variantId
   HAVING ? OR COUNT(ExternalTimer.implId) = MAX(Impls.count)
)|]
        }
      ]

    params :: [PersistValue]
    params = []

    queryText = [i|
SELECT VariantTiming.variantId
     , OptimalStep.optimal
     , VariantTiming.bestNonSwitching
     , VariantTiming.timings
     , ExternalTiming.timings
FROM VariantTiming

INNER JOIN OptimalStep
ON VariantTiming.variantId = OptimalStep.variantId

LEFT JOIN ExternalTiming
ON VariantTiming.variantId = ExternalTiming.variantId

ORDER BY VariantTiming.variantId ASC|]
