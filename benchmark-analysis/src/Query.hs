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
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQueryCount
    , propertyQuery
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

explainSqlQuery :: Query r -> SqlM ()
explainSqlQuery originalQuery =
    runSqlQuery explainQuery $ C.mapM_ (liftIO . T.putStrLn)
  where
    explain
        :: (MonadIO m, MonadLogger m, MonadThrow m) => [PersistValue] -> m Text
    explain [PersistInt64 _,PersistInt64 _,PersistInt64 _,PersistText t] =
        return t
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
runSqlQuery Query{..} sink = do
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
    { props :: {-# UNPACK #-} !(Vector Double)
    , bestImpl :: {-# UNPACK #-} !Int64
    , variantId :: {-# UNPACK #-} !(Key Variant)
    , stepId :: {-# UNPACK #-} !Int64
    , timings :: {-# UNPACK #-} !(Vector Double)
    } deriving (Show)

--FIXME specify algorithm?!
propertyQuery :: Key GPU -> Set Text -> Set Text -> Query StepInfo
propertyQuery gpuId graphProps stepProps = Query{..}
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
            , PersistInt64 bestImpl, PersistInt64 (toSqlKey -> variantId)
            , PersistInt64 stepId, PersistByteString times
            ] = return $ StepInfo{..}
      where
        props = byteStringToVector bs1 <> byteStringToVector bs2
        timings = byteStringToVector times

    convert l = logThrowM . Error . fromString $ "Unexpected value: " ++ show l

    tempTables = [[i|
CREATE TEMP TABLE indexedGraphProps AS
    SELECT DISTINCT property
    FROM GraphProp
    WHERE #{whereClauses graphProps}
    ORDER BY property ASC
|],[i|
CREATE TEMP TABLE indexedStepProps AS
    SELECT DISTINCT property
    FROM StepProp
    WHERE #{whereClauses stepProps}
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
            , vector(value, idxProps.rowid, #{S.size graphProps}) AS props
       FROM GraphProp
       INNER JOIN indexedGraphProps AS idxProps
             ON idxProps.property = GraphProp.property
       GROUP BY graphId) AS GraphProps
     ON GraphProps.graphId = Graph.id
     INNER JOIN
     ( SELECT variantId, stepId
            , vector(value, idxProps.rowid, #{S.size stepProps}) AS props
       FROM StepProp
       INNER JOIN indexedStepProps AS idxProps
             ON idxProps.property = StepProp.property
       GROUP BY variantId, stepId) AS StepProps
     ON Variant.id = StepProps.variantId AND Step.stepId = StepProps.stepId
ORDER BY Variant.id, Step.stepId ASC|]
