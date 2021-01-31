{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Predictor
    ( CookedPredictor(predictorId)
    , Index(..)
    , MispredictionStrategy
    , Model.Model
    , PredictorConfig
    , RawPredictor(rawPredictorId,rawPredictorName)
    , RawPrediction(..)
    , Model.byteStringToModel
    , cookPredictor
    , getPredictorConfigAlgorithmId
    , predictorToCxx
    , loadPredictor
    , predict
    , predictCooked
    , rawPredict
    , toPredictorName
    ) where

import Control.Monad (when)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable, Vector)

import Core
import qualified Model
import Model.Stats (ModelStats(..), UnknownSet(..), getModelStats)
import Predictor.Config
import Predictor.Raw (RawPredictor(..), predictorToCxx)
import Schema
import Sql (MonadSql, SqlBackend, SqlRecord, ToBackendKey)
import qualified Sql
import Utils.ImplTiming (ImplTiming(..))
import Utils.PropValue (PropValue(..))

newtype Index = Index { getIdx :: Int } deriving (Eq, Storable)

toPredictorName :: MonadSql m => Key PredictionModel -> m (Int, Text)
toPredictorName modelId = do
    predName <- getModelName <$> Sql.getJust modelId
    return (getPredictorImplId modelId, predName)

data RawPrediction
    = ImplPrediction Int
    | RefinedPrediction Int64 UnknownSet
    | MisPredictionSet UnknownSet
    | Unknown

rawPredict :: RawPredictor -> Vector Double -> RawPrediction
rawPredict RawPredictor{..} props
    | modelPrediction >= 0 = ImplPrediction modelPrediction
    | otherwise = case rawPredictorUnknownSets !? unknownSetId of
        Just s | refinedPrediction < 0 -> MisPredictionSet s
               | otherwise -> RefinedPrediction refinedPrediction s
        Nothing -> Unknown
  where
    modelPrediction = Model.predict rawPredictorModel props
    refinedPrediction = fromIntegral $
        rawPredictorMispredictionStrategy modelPrediction
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
    :: Text
    -> Int
    -> (Map Int64 UnknownSet -> Int -> Int)
    -> Key PredictionModel
    -> SqlM RawPredictor
rawLoad rawPredictorName defaultImpl strategy rawPredictorId = do
    PredictionModel{..} <- Sql.getJust rawPredictorId
    ModelStats{modelUnknownPreds} <- getModelStats rawPredictorId

    return RawPredictor
      { rawPredictorModel = predictionModelModel
      , rawPredictorUnknownSets = modelUnknownPreds
      , rawPredictorDefaultImpl = defaultImpl
      , rawPredictorMispredictionStrategy = strategy modelUnknownPreds
      , ..
      }

loadPredictor :: PredictorConfig -> SqlM RawPredictor
loadPredictor PConfig{..} = do
    PredictionModel{..} <- Sql.getJust pConfigModelId
    Implementation{..} <- Sql.getJust pConfigDefaultImpl

    when (implementationAlgorithmId /= predictionModelAlgorithmId) $ do
        logThrowM $ GenericInvariantViolation
           "Default implementation algorithm and model algorithm do not match."

    lookupMisprediction <-
        getMispredictionStrategy pConfigModelId pConfigStrategy

    rawLoad pConfigModelName defaultImpl lookupMisprediction pConfigModelId
  where
    defaultImpl :: Int
    defaultImpl = fromIntegral $ fromSqlKey pConfigDefaultImpl

cookPredictor
    :: forall m . (MonadLogger m, MonadSql m, MonadThrow m)
    => Vector PropValue
    -> Vector ImplTiming
    -> RawPredictor
    -> m CookedPredictor
cookPredictor propVec implVec RawPredictor{..} = do
    ModelStats{..} <- getModelStats rawPredictorId
    idxForPropMap <-
        V.ifoldM' (buildLookup propValuePropId) M.empty propVec

    (idxForImplMap :: Map (Key Implementation) Int) <-
        V.ifoldM' (buildLookup implTimingImpl) M.empty implVec

    propIdxMap <- case swapMap modelPropImportance of
        Just lut -> return lut
        Nothing -> logThrowM $ GenericInvariantViolation
            "Encountered duplicate properties!"

    let translateProp :: Int -> m Int
        translateProp i = do
            propId <- case M.lookup i propIdxMap of
                Nothing -> logThrowM . GenericInvariantViolation $
                    "Unable to lookup property: " <> showText i
                Just v -> return v

            case M.lookup propId idxForPropMap of
                Nothing -> logThrowM . GenericInvariantViolation $
                    "Unable to lookup property index: " <> showText i
                Just v -> return v

        translateImplementations :: Int -> m Int
        translateImplementations i
            | impl == -1 = return impl
            | otherwise = do
                implId <- Sql.validateKey $ fromIntegral impl

                case M.lookup implId idxForImplMap of
                    Nothing -> logThrowM . GenericInvariantViolation $
                        "Unable to lookup implementation index: " <> showText i
                    Just v -> return v
          where
            impl | i >= -1 = i
                 | otherwise = rawPredictorMispredictionStrategy i

    defaultImpl <- Sql.validateKey $
        fromIntegral rawPredictorDefaultImpl

    newDefaultImpl <- case M.lookup defaultImpl idxForImplMap of
        Nothing -> logThrowM . GenericInvariantViolation $
            "Unable to lookup implementation index: "
            <> showText rawPredictorDefaultImpl
        Just v -> return (Index v)

    propModel <- Model.mapPropIndices translateProp rawPredictorModel
    newModel <- Model.mapImplementations translateImplementations propModel

    return $ CookedPredictor
        { predictorId = rawPredictorId
        , cookedPredictorModel = newModel
        , cookedPredictorDefaultImpl = newDefaultImpl
        }
  where
    buildLookup
        :: (Ord (Key k), SqlRecord k, ToBackendKey SqlBackend k)
        => (v -> Int64)
        -> Map (Key k) Int
        -> Int
        -> v
        -> m (Map (Key k) Int)
    buildLookup f lut i val = do
        key <- Sql.validateKey $ f val

        when (key `M.member` lut) . logThrowM $ GenericInvariantViolation
            "Found duplicate property id in input!"

        return $ M.insert key i lut

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

data CookedPredictor = CookedPredictor
     { predictorId :: Key PredictionModel
     , cookedPredictorModel :: Model
     , cookedPredictorDefaultImpl :: Index
     }

predictCooked :: CookedPredictor -> Vector PropValue -> Maybe Index -> Index
predictCooked CookedPredictor{..} props old
    | prediction >= 0 = Index prediction
    | otherwise = case old of
        Nothing -> cookedPredictorDefaultImpl
        Just v -> v
  where
    prediction = Model.predictPropVector cookedPredictorModel props
