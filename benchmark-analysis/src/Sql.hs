{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sql (module Sql.Core, module Sql) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Conduit (MonadIO, MonadResource, (.|), runConduit)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.String.Interpolate.IsString (i)

import Database.Persist.Sqlite (getFieldName, getTableName, liftPersist)
import Exceptions
import Query (MonadQuery, Query(..), runSqlQuerySingle)
import Schema
import Sql.Core hiding (executeSql, liftProjectPersist)

newtype Avg = Avg { getAvg :: Int } deriving (Show, Eq, Ord)
newtype Max = Max { getMax :: Int } deriving (Show, Eq, Ord)

getFieldLength
    :: forall a m rec
     . (MonadQuery m, SqlRecord rec)
    => EntityField rec a -> m (Avg, Max)
getFieldLength entityField = do
    table <- liftPersist $ getTableName (undefined :: rec)
    field <- liftPersist $ getFieldName entityField
    let queryText = [i|
SELECT ROUND(AVG(length(#{table}.#{field})))
     , MAX(length(#{table}.#{field}))
FROM #{table}
|]
    runSqlQuerySingle Query{..}
  where
    queryName :: Text
    queryName = "getMaxFieldQuery"

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params = []

    convert
        :: (MonadIO n, MonadLogger n, MonadThrow n)
        => [PersistValue] -> n (Avg, Max)
    convert [avgPersistVal, maxPersistVal]
        | Right avgVal <- fromPersistValue avgPersistVal
        , Right maxVal <- fromPersistValue maxPersistVal
        = return (Avg avgVal, Max maxVal)
    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [SqlInt64, SqlInt64]

queryExternalImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId = runConduit $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadResource m, MonadSql m)
    => Key Algorithm -> m (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduit $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

    mkImpl :: Text -> Text -> Implementation
    mkImpl short long = Implementation algoId short (Just long) Nothing Builtin

    builtinImpls :: IntMap Implementation
    builtinImpls = IM.fromList
        [ (predictedImplId, mkImpl "predicted" "Predicted")
        , (bestNonSwitchingImplId, mkImpl "best" "Best Non-switching")
        , (optimalImplId, mkImpl "optimal" "Optimal")
        ]
