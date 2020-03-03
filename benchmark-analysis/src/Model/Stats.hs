{-# LANGUAGE RecordWildCards #-}
module Model.Stats (ModelStats(..), UnknownSet(..), getModelStats) where

import Control.Monad (forM)
import qualified Data.Conduit.Combinators as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set
import qualified Data.Set as S

import Core
import Schema
import Sql ((==.))
import qualified Sql.Transaction as SqlTrans

data UnknownSet = UnknownSet
    { unknownSetOccurence :: Int
    , unknownSetImpls :: Set (Key Implementation)
    } deriving (Eq, Show)

data ModelStats = ModelStats
    { modelGraphPropImportance :: Map Text Double
    , modelStepPropImportance :: Map Text Double
    , modelUnknownCount :: Int
    , modelUnknownPreds :: Map Int64 UnknownSet
    } deriving (Eq, Show)

getModelStats :: Key PredictionModel -> SqlM ModelStats
getModelStats modelId = SqlTrans.runTransaction $ do
    modelUnknownCount <-
        predictionModelTotalUnknownCount <$> SqlTrans.getJust modelId

    modelGraphPropImportance <-
        SqlTrans.selectSource [ModelGraphPropertyModelId ==. modelId] [] $
            C.foldMap (graphPropMap . SqlTrans.entityVal)

    modelStepPropImportance <-
        SqlTrans.selectSource [ModelStepPropertyModelId ==. modelId] [] $
            C.foldMap (stepPropMap . SqlTrans.entityVal)

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
    graphPropMap ModelGraphProperty{..} =
      M.singleton modelGraphPropertyProperty modelGraphPropertyImportance

    stepPropMap ModelStepProperty{..} =
      M.singleton modelStepPropertyProperty modelStepPropertyImportance

    toImplSet UnknownPredictionSet{..} = S.singleton unknownPredictionSetImplId
