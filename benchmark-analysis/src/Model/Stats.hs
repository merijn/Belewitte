{-# LANGUAGE RecordWildCards #-}
module Model.Stats
    ( ModelStats(..)
    , UnknownSet(..)
    , getModelStats
    , implInUnknownSet
    ) where

import Control.Monad (forM)
import qualified Data.Conduit.Combinators as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set
import qualified Data.Set as S

import Core
import Schema
import Sql (MonadSql, (==.))
import qualified Sql.Transaction as SqlTrans

data UnknownSet = UnknownSet
    { unknownSetOccurence :: Int
    , unknownSetImpls :: Set (Key Implementation)
    } deriving (Eq, Show)

implInUnknownSet :: Integral n => n -> UnknownSet -> Bool
implInUnknownSet n = S.member implKey . unknownSetImpls
  where
    implKey = toSqlKey $ fromIntegral n

data ModelStats = ModelStats
    { modelPropImportance :: Map (Key PropertyName) (Int, Double)
    , modelUnknownCount :: Int
    , modelUnknownPreds :: Map Int64 UnknownSet
    } deriving (Eq, Show)

getModelStats :: MonadSql m => Key PredictionModel -> m ModelStats
getModelStats modelId = SqlTrans.runTransaction $ do
    modelUnknownCount <-
        predictionModelTotalUnknownCount <$> SqlTrans.getJust modelId

    modelPropImportance <-
        SqlTrans.selectSource [ModelPropertyModelId ==. modelId] [] $
            C.foldMap propNameMap

    unknowns <- SqlTrans.selectList [UnknownPredictionModelId ==. modelId] []
    unknownPreds <- forM unknowns $ \SqlTrans.Entity{..} -> do
        let UnknownPrediction{..} = entityVal
            filters = [UnknownPredictionSetUnknownPredId ==. entityKey]

        implSet <- SqlTrans.selectSource filters [] $
            C.foldMap (toImplSet . SqlTrans.entityVal)

        return $ M.singleton unknownPredictionUnknownSetId
                             (UnknownSet unknownPredictionCount implSet)

    return ModelStats{modelUnknownPreds = mconcat unknownPreds, ..}
  where
    propNameMap :: Entity ModelProperty -> Map (Key PropertyName) (Int, Double)
    propNameMap (Entity _ ModelProperty{..}) = M.singleton modelPropertyPropId
        (modelPropertyPropertyIdx, modelPropertyImportance)

    toImplSet UnknownPredictionSet{..} = S.singleton unknownPredictionSetImplId
