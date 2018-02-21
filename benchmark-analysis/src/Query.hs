{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Query
    ( Query
    , explainSqlQuery
    , randomizeQuery
    , runSqlQuery
    , runSqlQueryCount
    , propertyQuery
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Resource (release)
import Data.Acquire (allocateAcquire)
import Data.Conduit (ConduitT, Void, (.|), await, runConduit)
import qualified Data.Conduit.List as C
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String.Interpolate.IsString
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Database.Persist.Sqlite

import Schema

data Query r =
  Query
    { params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadLogger m, MonadThrow m)
              => [PersistValue] -> m r
    , query :: Text
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

data Error = Error Text deriving (Show)
instance Exception Error

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
    srcRes <- liftPersist $ rawQueryRes query params
    (key, src) <- allocateAcquire srcRes
    runConduit (src .| C.mapM convert .| sink) <* release key

runSqlQueryCount :: Query r -> SqlM Int
runSqlQueryCount Query{params,query} = do
    result <- liftPersist $ withRawQuery countQuery params await
    case result of
        Just [PersistInt64 n] -> return $ fromIntegral n
        _ -> logThrowM $ Error "Error computing count!"
  where
    countQuery = "SELECT COUNT(*) FROM (" <> query <> ")"

propertyQuery
    :: Key GPU -> Set Text -> Set Text -> Query (Vector Double, Int64)
propertyQuery gpuId graphProps stepProps = Query{..}
  where
    query = [i|
SELECT #{selectClause}
FROM Variant
     INNER JOIN Graph ON Variant.graphId = Graph.id
     INNER JOIN
     ( SELECT variantId, stepId, implId
       FROM StepTimer
       INNER JOIN Implementation ON StepTimer.implId = Implementation.id
       WHERE gpuId = ? AND Implementation.type = "Core"
       GROUP BY variantId, stepId
       HAVING avgTime = MIN(avgTime)) AS Step
     ON Variant.id = Step.variantId
#{genClauses joinClause}
WHERE #{mconcat . intersperse " AND " $ genClauses whereClause}
ORDER BY Graph.name, Variant.name, Step.stepId ASC|]

    params :: [PersistValue]
    params = [toPersistValue gpuId]

    convert
        :: (MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (Vector Double, Int64)
    convert = go id
      where
        go f [PersistInt64 n] = return (V.fromList (f []), n)
        go f (PersistDouble d:vs) = go (f . (d:)) vs
        go _ _ = logThrowM . Error $ "Unexpected value!"

    selectClause :: Text
    selectClause = genClauses select <> "Step.implId"

    genClauses :: Monoid m => (Text -> (Word, Text) -> m) -> m
    genClauses f =
        clause "GraphProp" graphProps <> clause "StepProp" stepProps
      where
        clause table = foldMap (f table) . zip [0..] . S.toAscList

    select :: Text -> (Word, a) -> Text
    select table (n, _) = [i|#{table}#{n}.value, |]

    joinClause :: Text -> (Word, a) -> Text
    joinClause tbl (n, _) = case tbl of
        "GraphProp" -> clause [i|Variant.graphId = #{tbl}#{n}.graphId|]
        "StepProp" -> clause $ [i|Variant.id = #{tbl}#{n}.variantId|]
                            <> [i| AND #{tbl}#{n}.stepId = Step.stepId|]
        _ -> ""
      where
        clause s =
            [i|     INNER JOIN #{tbl} AS #{tbl}#{n} ON |] <> s <> "\n"

    whereClause :: Text -> (Word, Text) -> [Text]
    whereClause table (n, prop) = [[i|#{table}#{n}.property = '#{prop}'|]]
