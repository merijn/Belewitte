{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Query
    ( Algorithms
    , Props
    , Query(Query,columnCount)
    , explainSqlQuery
    , runSqlQueryCount
    , runSqlQuery
    , runSqlQueryRandom
    , propertyQuery
    ) where

import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Trans.Resource
import Data.Acquire (allocateAcquire)
import Data.Conduit
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

type Props = Vector Double
type Algorithms = Int64

data Query r =
  Query
    { columnCount :: Int
    , params :: [PersistValue]
    , convert :: forall m . (MonadIO m, MonadThrow m) => [PersistValue] -> m r
    , query :: Text
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

data Error = Error Text deriving (Show)
instance Exception Error

explainSqlQuery :: Query r -> SqlM ()
explainSqlQuery q@Query{} = do
    runConduit $ runSqlQuery newQuery .| C.mapM_ (liftIO . T.putStrLn)
  where
    explain :: (MonadIO m, MonadThrow m) => [PersistValue] -> m Text
    explain [PersistInt64 _,PersistInt64 _,PersistInt64 _,PersistText t] =
        return t
    explain _ = throwM $ Error "Explain failed!"

    newQuery = q{ query = "EXPLAIN QUERY PLAN " <> query q, convert = explain }

runSqlQueryCount :: Query r -> SqlM Int
runSqlQueryCount Query{params,query} = do
    result <- runSql $ withRawQuery countQuery params await
    case result of
        Just [PersistInt64 n] -> return $ fromIntegral n
        _ -> throwM $ Error "Error computing count!"
  where
    countQuery = "SELECT COUNT(*) FROM (" <> query <> ")"

runSqlQueryRandom :: Double -> Query r -> Source SqlM r
runSqlQueryRandom fraction q = do
    num <- lift $ runSqlQueryCount q

    let trainingSize :: Integer
        trainingSize = round (fraction * fromIntegral num)

        queryLimit = [i|LIMIT #{trainingSize}|]

        randomizedQuery = [i|SELECT * FROM (#{query q}) ORDER BY random() |]

    runSqlQuery q{ query = randomizedQuery <> queryLimit }

runSqlQuery :: Query r -> Source SqlM r
runSqlQuery Query{..} = do
    srcRes <- lift . runSql $ rawQueryRes query params
    (_, src) <- allocateAcquire srcRes
    src .| converter
  where
      converter = awaitForever $ convert >=> yield

propertyQuery :: Key GPU -> Set Text -> Set Text -> Query (Props, Algorithms)
propertyQuery gpuId graphProps stepProps = Query{..}
  where
    query = [i|
SELECT #{selectClause}
FROM Variant
     INNER JOIN Graph ON Variant.graphId = Graph.id
     INNER JOIN
     ( SELECT variantId, stepId, implId
       FROM StepTime
       WHERE gpuId = #{fromSqlKey gpuId}
       GROUP BY variantId, stepId
       HAVING avgRuntime = MIN(avgRuntime)) AS Step
     ON Variant.id = Step.variantId
#{genClauses joinClause}
WHERE #{mconcat . intersperse " AND " $ genClauses whereClause}
ORDER BY Graph.name, Variant.variant, Step.stepId ASC|]

    params :: [PersistValue]
    params = []

    columnCount :: Int
    columnCount = S.size graphProps + S.size stepProps

    convert :: MonadThrow m => [PersistValue] -> m (Props, Algorithms)
    convert = go id columnCount
      where
        go f 0 [PersistInt64 n] = return (V.fromListN columnCount (f []), n)
        go f n (PersistDouble d:vs) = go (f . (d:)) (n-1) vs
        go _ _ _ = throwM . Error $ "Unexpect value!"

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
