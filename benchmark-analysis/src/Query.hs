{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Query
    ( Query
    , StepInfo(..)
    , VariantInfo(..)
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQueryCount
    , stepInfoQuery
    , variantInfoQuery
    , timePlotQuery
    , levelTimePlotQuery
    ) where

import Control.Monad (forM_, join)
import Control.Monad.IO.Unlift (toIO)
import Control.Monad.Trans.Resource (allocate, release)
import Data.Acquire (allocateAcquire)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.List as C
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.String.Interpolate.IsString (i)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import Database.Persist.Sqlite
import System.IO (Handle)

import Core
import Schema
import Utils (byteStringToVector)

data Query r =
  Query
    { tempTables :: [Text]
    , clearTempTables :: [Text]
    , params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadLogger m, MonadThrow m)
              => [PersistValue] -> m r
    , query :: Text
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

explainSqlQuery :: Query r -> Handle -> SqlM ()
explainSqlQuery originalQuery hnd = do
    runSqlQuery' explainQuery $ C.mapM_ (liftIO . T.hPutStrLn hnd)
    liftIO $ do
        T.hPutStrLn hnd $ ""
        T.hPutStrLn hnd $ T.replicate 80 "#"
        T.hPutStrLn hnd $ ""
  where
    explain
        :: (MonadIO m, MonadLogger m, MonadThrow m) => [PersistValue] -> m Text
    explain
        [PersistInt64 _, PersistInt64 _, PersistInt64 _, PersistText t]
        = return t
    explain _ = logThrowM $ Error "Explain failed!"

    explainQuery = originalQuery
      { query = "EXPLAIN QUERY PLAN " <> query originalQuery
      , convert = explain
      }

randomizeQuery :: Int -> Integer -> Query r -> (Query r, Query r)
randomizeQuery seed trainingSize originalQuery = (training, validation)
  where
    randomizedQuery =
      [i|SELECT * FROM (#{query originalQuery}) ORDER BY random(#{seed}) |]

    training = originalQuery
      { query = randomizedQuery <> [i|LIMIT #{trainingSize}|] }

    validation = originalQuery
      { query = randomizedQuery <> [i|LIMIT -1 OFFSET #{trainingSize}|] }

runSqlQuery :: Query r -> ConduitT r Void SqlM a -> SqlM a
runSqlQuery query sink = do
    logQueryExplanation $ explainSqlQuery query
    runSqlQuery' query sink

runSqlQuery' :: Query r -> ConduitT r Void SqlM a -> SqlM a
runSqlQuery' Query{..} sink = do
    (tables, ()) <- join $ allocate <$> createTables <*> dropTables
    srcRes <- liftPersist $ rawQueryRes query params
    (key, src) <- allocateAcquire srcRes
    runConduit (src .| C.mapM convert .| sink) <* release key <* release tables
  where
    createTables :: SqlM (IO ())
    createTables = toIO . forM_ tempTables $ \q -> rawExecute q []

    dropTables :: SqlM (() -> IO ())
    dropTables = const <$> act
      where
        act :: SqlM (IO ())
        act = toIO . forM_ clearTempTables $ \q -> rawExecute q []

runSqlQueryCount :: Query r -> SqlM Int
runSqlQueryCount originalQuery = do
    result <- runSqlQuery countQuery await
    case result of
        Just n -> return n
        Nothing -> logThrowM . Error $ "Missing count result!"
  where
    countQuery = originalQuery
        { query = "SELECT COUNT(*) FROM (" <> query originalQuery <> ")"
        , convert = \case
            [PersistInt64 n] -> return $ fromIntegral n
            _ -> logThrowM . Error $ "Unexpected value in count query"
        }

data StepInfo =
  StepInfo
    { stepProps :: {-# UNPACK #-} !(Vector Double)
    , stepBestImpl :: {-# UNPACK #-} !Int64
    , stepVariantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , stepTimings :: {-# UNPACK #-} !(Vector Double)
    } deriving (Show)

--FIXME specify algorithm?!
stepInfoQuery :: Key GPU -> Set Text -> Set Text -> Query StepInfo
stepInfoQuery gpuId graphProperties stepProperties = Query{..}
  where
    params :: [PersistValue]
    params = [toPersistValue gpuId]

    whereClauses :: Set Text -> Text
    whereClauses = T.intercalate " OR " . map clause . S.toAscList
      where
        clause t = [i|property = "#{t}"|]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m StepInfo
    convert [ PersistByteString bs1, PersistByteString bs2
            , PersistInt64 stepBestImpl
            , PersistInt64 (toSqlKey -> stepVariantId) , PersistInt64 stepId
            , PersistByteString times
            ] = return $ StepInfo{..}
      where
        stepProps = byteStringToVector bs1 <> byteStringToVector bs2
        stepTimings = byteStringToVector times

    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    tempTables = [[i|
CREATE TEMP TABLE indexedGraphProps AS
    SELECT DISTINCT property
    FROM GraphProp
    WHERE #{whereClauses graphProperties}
    ORDER BY property ASC
|],[i|
CREATE TEMP TABLE indexedStepProps AS
    SELECT DISTINCT property
    FROM StepProp
    WHERE #{whereClauses stepProperties}
    ORDER BY property ASC
|]]

    clearTempTables =
      ["DROP TABLE indexedGraphProps","DROP TABLE indexedStepProps"]

    query = [i|
SELECT GraphProps.props, StepProps.props, Step.implId, Variant.id, Step.stepId, Step.timings
FROM Variant
INNER JOIN Graph ON Variant.graphId = Graph.id
INNER JOIN
    ( SELECT variantId, stepId, implId
           , MIN(CASE Implementation.type
                 WHEN "Core" THEN avgTime
                 ELSE NULL END
           )
           , vector(avgTime, implId, (SELECT COUNT(*) FROM Implementation)) AS timings
      FROM StepTimer
      INNER JOIN Implementation ON StepTimer.implId = Implementation.id
      WHERE gpuId = ?
      GROUP BY variantId, stepId
    ) AS Step
    ON Variant.id = Step.variantId
INNER JOIN
    ( SELECT graphId
           , vector(value, idxProps.rowid, #{S.size graphProperties}) AS props
      FROM GraphProp
      INNER JOIN indexedGraphProps AS idxProps
          ON idxProps.property = GraphProp.property
      GROUP BY graphId
    ) AS GraphProps
    ON GraphProps.graphId = Graph.id
INNER JOIN
    ( SELECT variantId, stepId
           , vector(value, idxProps.rowid, #{S.size stepProperties}) AS props
      FROM StepProp
      INNER JOIN indexedStepProps AS idxProps
          ON idxProps.property = StepProp.property
      GROUP BY variantId, stepId
    ) AS StepProps
    ON Variant.id = StepProps.variantId AND Step.stepId = StepProps.stepId
ORDER BY Variant.id, Step.stepId ASC|]

data VariantInfo =
  VariantInfo
    { variantId :: {-# UNPACK #-} !(Key Variant)
    , variantOptimal :: {-# UNPACK #-} !Double
    , variantBestNonSwitching :: {-# UNPACK #-} !Double
    , variantTimings :: {-# UNPACK #-} !(Vector Double)
    } deriving (Show)

variantInfoQuery :: Key GPU -> Query VariantInfo
variantInfoQuery gpuId = Query{..}
  where
    params :: [PersistValue]
    params = [toPersistValue gpuId, toPersistValue gpuId]

    tempTables = []
    clearTempTables = []

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m VariantInfo
    convert [ PersistInt64 (toSqlKey -> variantId)
            , PersistDouble variantOptimal
            , PersistDouble variantBestNonSwitching
            , PersistByteString (byteStringToVector -> variantTimings) ]
            = return $ VariantInfo{..}

    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    query = [i|
SELECT Variant.id, OptimalStep.optimal, Total.bestNonSwitching, Total.timings
FROM Variant
INNER JOIN
    ( SELECT variantId, SUM(Step.minTime) AS optimal
      FROM ( SELECT variantId, stepId
                  , MIN(CASE Implementation.type
                        WHEN "Core" THEN avgTime
                        ELSE NULL END) as minTime
             FROM StepTimer
             INNER JOIN Implementation ON StepTimer.implId = Implementation.id
             WHERE gpuId = ?
             GROUP BY variantId, stepId) AS Step
      GROUP BY variantId
    ) AS OptimalStep
    ON Variant.id = OptimalStep.variantId
INNER JOIN
    ( SELECT variantId
           , MIN(CASE Implementation.type
                 WHEN "Core" THEN avgTime
                 ELSE NULL END) as bestNonSwitching
           , vector(avgTime, implId, (SELECT COUNT(*) FROM Implementation)) AS timings
      FROM TotalTimer
      INNER JOIN Implementation ON TotalTimer.implId = Implementation.id
      WHERE gpuId = ? AND TotalTimer.name = "computation"
      GROUP BY variantId
    ) AS Total
    ON Variant.id = Total.variantId
ORDER BY Variant.id ASC|]

timePlotQuery :: Key GPU -> Set (Key Variant) -> Query (Text, Vector Double)
timePlotQuery gpuId variants = Query{..}
  where
    params :: [PersistValue]
    params = [toPersistValue gpuId]

    tempTables = []
    clearTempTables = []

    havingClause = T.intercalate " OR " . map clause . S.toList
      where
        clause k = [i|variantId = #{fromSqlKey k}|]

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Text, Vector Double)
    convert [PersistText graph, PersistByteString timings] =
        return $ (graph, byteStringToVector timings)
    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    query = [i|
SELECT Graph.name, Total.timings
FROM Variant
INNER JOIN Graph ON Variant.graphId = Graph.id
INNER JOIN
    ( SELECT variantId
           , vector(avgTime, implId, (SELECT COUNT(*) FROM Implementation)) AS timings
      FROM TotalTimer
      WHERE gpuId = ? AND name = "computation"
      GROUP BY variantId
      HAVING #{havingClause variants}
    ) AS Total
    ON Variant.id = Total.variantId
WHERE Variant.name = "default"
ORDER BY Variant.id ASC|]

levelTimePlotQuery :: Key GPU -> Key Variant -> Query (Int64, Vector Double)
levelTimePlotQuery gpuId variant = Query{..}
  where
    params :: [PersistValue]
    params = [toPersistValue gpuId, toPersistValue variant]

    tempTables = []
    clearTempTables = []

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Int64, Vector Double)
    convert [PersistInt64 stepId, PersistByteString timings] =
        return $ (stepId, byteStringToVector timings)
    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    query = [i|
SELECT stepId, vector(avgTime, implId, (SELECT COUNT(*) FROM Implementation)) AS timings
FROM StepTimer
WHERE gpuId = ? AND variantId = ?
GROUP BY stepId
ORDER BY stepId ASC|]
