{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor
    ( CookedPredictor(predictorId)
    , MispredictionStrategy(..)
    , RawPredictor(rawPredictorId)
    , RawPrediction(..)
    , cookPredictor
    , loadPredictor
    , predict
    , predictCooked
    , rawPredict
    , toPredictorName
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

toPredictorName :: MonadSql m => Key PredictionModel -> m (Int, Text)
toPredictorName modelId = do
    predName <- getModelName <$> Sql.getJust modelId
    return (predictedImplId - fromIntegral (fromSqlKey modelId), predName)

data MispredictionStrategy
    = None
    | DefImpl (Either Int Text)

data RawPredictor = RawPredictor
    { rawPredictorId :: Key PredictionModel
    , rawPredictorModel :: Model
    , rawPredictorUnknownSets :: Map Int64 UnknownSet
    , rawPredictorDefaultImpl :: Int
    , rawPredictorMispredictionStrategy :: Int -> Int
    }

data RawPrediction
    = ImplPrediction Int
    | MisPredictionSet UnknownSet
    | Unknown

rawPredict :: RawPredictor -> Vector Double -> RawPrediction
rawPredict RawPredictor{..} props
    | modelPrediction >= 0 = ImplPrediction modelPrediction
    | otherwise = case rawPredictorUnknownSets !? unknownSetId of
        Just s -> MisPredictionSet s
        Nothing -> Unknown
  where
    modelPrediction = Model.predict rawPredictorModel props
    unknownSetId = negate $ fromIntegral modelPrediction

predict :: RawPredictor -> Vector Double -> Maybe Int -> Int
predict RawPredictor{..} props lastImpl
    | modelPrediction >= 0 = modelPrediction
    | disambiguatedPrediction /= -1 = disambiguatedPrediction
    | otherwise = case lastImpl of
        Just i -> i
        Nothing -> rawPredictorDefaultImpl
  where
    modelPrediction = Model.predict rawPredictorModel props
    disambiguatedPrediction = rawPredictorMispredictionStrategy modelPrediction

rawLoad
    :: Int -> (Int -> Int) -> Key PredictionModel -> SqlM RawPredictor
rawLoad defaultImpl strategy rawPredictorId = do
    PredictionModel{..} <- Sql.getJust rawPredictorId
    ModelStats{modelUnknownPreds} <- getModelStats rawPredictorId

    return RawPredictor
      { rawPredictorModel = predictionModelModel
      , rawPredictorUnknownSets = modelUnknownPreds
      , rawPredictorDefaultImpl = defaultImpl
      , rawPredictorMispredictionStrategy = strategy
      , ..
      }

loadPredictor
    :: Key PredictionModel -> MispredictionStrategy -> SqlM RawPredictor
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

cookPredictor
    :: (MonadLogger m, MonadSql m, MonadThrow m)
    => Vector PropValue -> RawPredictor -> m CookedPredictor
cookPredictor propVec RawPredictor{..} = do
    ModelStats{..} <- getModelStats rawPredictorId
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
    return $ CookedPredictor
        { predictorId = rawPredictorId
        , cookedPredictorModel = rawPredictorModel
        , cookedPredictorDefaultImpl = rawPredictorDefaultImpl
        }
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

data CookedPredictor = CookedPredictor
     { predictorId :: Key PredictionModel
     , cookedPredictorModel :: Model
     , cookedPredictorDefaultImpl :: Int
     }

predictCooked :: CookedPredictor -> Vector PropValue -> Maybe Int -> Int
predictCooked CookedPredictor{..} props old
    | prediction >= 0 = prediction
    | otherwise = case old of
        Nothing -> cookedPredictorDefaultImpl
        Just v -> v
  where
    prediction = Model.predictPropVector cookedPredictorModel props
