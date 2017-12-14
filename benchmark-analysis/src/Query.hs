{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Query
    ( Algorithms
    , Props
    , Query(Query,columnCount)
    , runSqlQueryCount
    , runSqlQuery
    , propertyQuery
    ) where

import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource
import Data.Acquire (allocateAcquire)
import Data.Conduit
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.String.Interpolate.IsString
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
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
    , selectClause :: Text
    , convert :: forall m . MonadThrow m => [PersistValue] -> m r
    , query :: Text -> Text
    }

instance Functor Query where
    fmap f query@Query{convert} = query { convert = fmap f . convert }

data Error = Error Text deriving (Show)
instance Exception Error

runSqlQueryCount :: Query r -> SqlM Int
runSqlQueryCount Query{params,query} = runSql $ do
    result <- withRawQuery (query "COUNT(*)") params await
    case result of
        Just [PersistInt64 n] -> return $ fromIntegral n
        _ -> throwM $ Error "Error computing count!"

runSqlQuery :: Query r -> Source SqlM r
runSqlQuery Query{..} = do
    srcRes <- lift . runSql $ rawQueryRes (query selectClause) params
    (_, src) <- allocateAcquire srcRes
    src .| converter
  where
      converter = awaitForever $ convert >=> yield

propertyQuery :: Key GPU -> Set Text -> Set Text -> Query (Props, Algorithms)
propertyQuery gpuId graphProps stepProps = Query{..}
  where
    query str = [i|
SELECT #{str}
FROM Variant
     INNER JOIN Graph ON Variant.graphId = Graph.id
     INNER JOIN StepProp AS Step ON Variant.id = Step.variantId
#{genClauses joinClause}
     INNER JOIN
     ( SELECT variantId, stepId, implId
       FROM StepTime
       WHERE gpuId = #{fromSqlKey gpuId} AND (implId = 1 OR implId = 4)
       GROUP BY variantId, stepId
       HAVING avgRuntime = MIN(avgRuntime)) AS Impl
     ON Variant.id = Impl.variantId AND Step.stepId = Impl.stepId
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
    selectClause = genClauses select <> "Impl.implId"

    genClauses :: Monoid m => (Text -> (Word, Text) -> m) -> m
    genClauses f =
        clause "GraphProp" graphProps <> clause "StepProp" stepProps
      where
        clause table = foldMap (f table) . zip [0..] . S.toList

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
