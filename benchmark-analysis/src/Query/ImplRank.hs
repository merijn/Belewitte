{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query.ImplRank (Column(..), Ranking(..), implRankQuery) where

import Data.String.Interpolate.IsString (i)
import Data.Vector.Storable (Vector)

import Core
import Query
import Query.Step
import Schema
import Utils.ImplTiming
import Utils.Vector (byteStringToVector)

data Column = MinTime | AvgTime | MaxTime
data Ranking = Min | Avg | Total

implRankQuery
    :: StepInfoConfig -> Column -> Ranking -> Query (Vector ImplTiming)
implRankQuery StepInfoConfig{..} column ranking =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "implRankQuery"

    columnName :: Text
    columnName = case column of
        MinTime -> "minTime"
        AvgTime -> "avgTime"
        MaxTime -> "maxTime"

    rankFunction :: Text
    rankFunction = case ranking of
        Min -> "MIN"
        Avg -> "AVG"
        Total -> "TOTAL"

    converter :: MonadConvert m => [PersistValue] -> m (Vector ImplTiming)
    converter [ PersistByteString (byteStringToVector -> vec) ] = return vec

    converter actualValues = logThrowM $
        QueryResultUnparseable actualValues [ SqlBlob ]

    commonTableExpressions :: [CTE]
    commonTableExpressions =
      [ [toPersistValue stepInfoAlgorithm] `inCTE` [i|
IndexedImpls(idx, implId, type, count) AS (
    SELECT ROW_NUMBER() OVER ()
         , Implementation.id
         , type
         , COUNT() OVER ()
    FROM Implementation
    WHERE algorithmId = ?
),

ImplVector(implValues) AS (
    SELECT init_key_value_vector(implId, idx, count)
    FROM IndexedImpls
)|]
      , CTE
        { cteParams =
            [ toPersistValue stepInfoTimestamp
            , toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoAlgorithm
            , toPersistValue stepInfoPlatform
            , toPersistValue stepInfoCommit
            ]
        , cteQuery = [i|
ImplValues(implId, value) AS (
    SELECT Run.implId, #{rankFunction}(#{columnName})
    FROM StepTimer

    INNER JOIN Run
    ON StepTimer.runId = Run.id

    INNER JOIN RunConfig
    ON Run.runConfigId = RunConfig.id

    WHERE Run.validated = 1
    AND Run.timestamp < ?
    AND Run.algorithmId = ?
    AND RunConfig.algorithmId = ?
    AND RunConfig.platformId = ?
    AND RunConfig.algorithmVersion = ?

    GROUP BY Run.implId
)|]
        }
      ]

    params :: [PersistValue]
    params = []

    queryText :: Text
    queryText = [i|
SELECT update_key_value_vector(implValues, idx, implId, value)
FROM ImplValues, ImplVector
INNER JOIN IndexedImpls AS Impls USING (implId)
|]
