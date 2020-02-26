{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor where

import Data.Maybe (listToMaybe)
import qualified Data.IntMap.Strict as IM
import Data.Vector.Storable (Vector)

import Core
import Model (Model)
import qualified Model
import Schema
import qualified Sql

data Predictor = Predictor
    { predictorModelId :: Key PredictionModel
    , predictorModel :: Model
    , defaultImplementation :: Int
    , mispredictionStrategy :: Int -> Int
    }

predict :: Predictor -> Vector Double -> Maybe Int -> Int
predict Predictor{..} props lastImpl
    | modelPrediction >= 0 = modelPrediction
    | disambiguatedPrediction /= -1 = disambiguatedPrediction
    | otherwise = case lastImpl of
        Just i -> i
        Nothing -> defaultImplementation
  where
    modelPrediction = Model.predict predictorModel props
    disambiguatedPrediction = mispredictionStrategy modelPrediction

loadPredictor :: Either Int Text -> Key PredictionModel -> SqlM Predictor
loadPredictor defImpl predictorModelId = do
    PredictionModel{..} <- Sql.getJust predictorModelId
    algorithm <- Sql.getJust predictionModelAlgorithmId
    impls <- Sql.queryImplementations predictionModelAlgorithmId

    let lookupByName :: Text -> Maybe Int
        lookupByName t = fmap fst
                        . listToMaybe
                        . filter ((t==) . implementationName . snd)
                        $ IM.toList impls

    defaultImplementation <- case defImpl of
        Left i | IM.member i impls -> return i
        Right t | Just i <- lookupByName t -> return i
        _ -> logThrowM $ UnexpectedMissingData
                "Default implementation not found for algorithm"
                (getAlgoName algorithm)
    return Predictor{predictorModel = predictionModelModel, ..}
  where
    mispredictionStrategy = const (-1)
