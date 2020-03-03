{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor
    ( Predictor
    , loadPredictor
    , predict
    , rawPredict
    ) where

import Data.Maybe (listToMaybe)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map, (!?))
import Data.Vector.Storable (Vector)

import Core
import Model (Model)
import qualified Model
import Model.Stats (ModelStats(..), UnknownSet, getModelStats)
import Schema
import qualified Sql

data Predictor = Predictor
    { predictorModelId :: Key PredictionModel
    , predictorModel :: Model
    , predictorUnknownSets :: Map Int64 UnknownSet
    , defaultImplementation :: Int
    , mispredictionStrategy :: Int -> Int
    }

data RawPrediction
    = ImplPrediction Int
    | MisPredictionSet UnknownSet
    | Unknown

rawPredict :: Predictor -> Vector Double -> RawPrediction
rawPredict Predictor{..} props
    | modelPrediction >= 0 = ImplPrediction modelPrediction
    | otherwise = case predictorUnknownSets !? fromIntegral modelPrediction of
        Just s -> MisPredictionSet s
        Nothing -> Unknown
  where
    modelPrediction = Model.predict predictorModel props

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

    ModelStats{modelUnknownPreds} <- getModelStats predictorModelId

    return Predictor
      { predictorModel = predictionModelModel
      , predictorUnknownSets = modelUnknownPreds
      , ..
      }
  where
    mispredictionStrategy = const (-1)
