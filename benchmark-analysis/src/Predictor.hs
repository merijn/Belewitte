{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor
    ( GeneralPredictor
    , MispredictionStrategy(..)
    , Predictor
    , RawPrediction(..)
    , loadPredictor
    , predict
    , predictGeneral
    , makeGeneralPredictor
    , rawPredict
    , rawPredictGeneral
    ) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS

import Core
import qualified Model
import Model.Stats (ModelStats(..), UnknownSet, getModelStats)
import Schema
import Sql (MonadSql)
import qualified Sql
import Utils.PropValue (PropValue(..))

data MispredictionStrategy
    = None
    | DefImpl (Either Int Text)

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
    | otherwise = case predictorUnknownSets !? unknownSetId of
        Just s -> MisPredictionSet s
        Nothing -> Unknown
  where
    modelPrediction = Model.predict predictorModel props
    unknownSetId = negate $ fromIntegral modelPrediction

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

rawLoad
    :: Int -> (Int -> Int) -> Key PredictionModel -> SqlM Predictor
rawLoad defaultImplementation mispredictionStrategy predictorModelId = do
    PredictionModel{..} <- Sql.getJust predictorModelId
    ModelStats{modelUnknownPreds} <- getModelStats predictorModelId

    return Predictor
      { predictorModel = predictionModelModel
      , predictorUnknownSets = modelUnknownPreds
      , ..
      }

loadPredictor :: Key PredictionModel -> MispredictionStrategy -> SqlM Predictor
loadPredictor modelId None = rawLoad (-1) (const (-1)) modelId

loadPredictor modelId (DefImpl defImpl) = do
    PredictionModel{..} <- Sql.getJust modelId
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

    rawLoad defaultImplementation (const (-1)) modelId

makeGeneralPredictor
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Predictor -> Vector PropValue -> m GeneralPredictor
makeGeneralPredictor predictor@Predictor{predictorModelId} propVec = do
    ModelStats{..} <- getModelStats predictorModelId
    generalIdxLUT <- V.ifoldM' buildLookup M.empty propVec
    propIdxLUT <- case swapMap modelPropImportance of
        Just lut -> return lut
        Nothing -> logThrowM $ GenericInvariantViolation
            "Encountered duplicate properties!"

    let vectorBuilder :: (MonadLogger m, MonadThrow m) => Int -> m Int
        vectorBuilder i
            | Just propId <- M.lookup i propIdxLUT
            , Just generalIdx <- M.lookup propId generalIdxLUT
            = return generalIdx

            | otherwise = logThrowM $ GenericInvariantViolation
                "Unable to determine property index!"

    lookupVec <- VS.generateM (M.size modelPropImportance) vectorBuilder
    return . GPredictor predictor $ fromLookupVector lookupVec
  where
    buildLookup
        :: (MonadLogger m, MonadSql m, MonadThrow m)
        => Map (Key PropertyName) Int
        -> Int
        -> PropValue
        -> m (Map (Key PropertyName) Int)
    buildLookup lut i PropValue{propValuePropId} = do
        propId <- Sql.validateKey "PropertyName" propValuePropId

        when (propId `M.member` lut) . logThrowM $ GenericInvariantViolation
            "Found duplicate property id in input!"

        return $ M.insert propId i lut

    swapMap
        :: Map (Key PropertyName) (Int, Double)
        -> Maybe (Map Int (Key PropertyName))
    swapMap = sequence . M.fromListWith merge . map swapKeyValue . M.toList
      where
        merge :: a -> b -> Maybe c
        merge _ _ = Nothing

        swapKeyValue
            :: (Key PropertyName, (Int, Double))
            -> (Int, Maybe (Key PropertyName))
        swapKeyValue (k, (val, _)) = (val, Just k)

    fromLookupVector :: Vector Int -> Vector PropValue -> Vector Double
    fromLookupVector idx props = VS.generate (VS.length idx) $
        propValueValue . VS.unsafeIndex props . VS.unsafeIndex idx

data GeneralPredictor = GPredictor
     { realPredictor :: Predictor
     , prepInput :: Vector PropValue -> Vector Double
     }

predictGeneral :: GeneralPredictor -> Vector PropValue -> Maybe Int -> Int
predictGeneral GPredictor{..} = predict realPredictor . prepInput

rawPredictGeneral :: GeneralPredictor -> Vector PropValue -> RawPrediction
rawPredictGeneral GPredictor{..} = rawPredict realPredictor . prepInput
