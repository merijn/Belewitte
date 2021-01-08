{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor.Config
    ( Column(..)
    , MispredictionStrategy(..)
    , PredictorConfig(..)
    , Ranking(..)
    , getMispredictionStrategy
    , getPredictorConfigAlgorithmId
    , mkPredictorConfig
    ) where

import Control.Monad (when)
import qualified Data.IntMap as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Semigroup (Arg, Min)
import qualified Data.Semigroup as Semi
import qualified Data.Vector.Generic as V

import Core
import Model.Stats (UnknownSet(..))
import Query (runSqlQuerySingle)
import Query.ImplRank (Column(..), Ranking(..), implRankQuery)
import Schema
import TrainConfig (getModelTrainingConfig, getStepInfoConfig)
import Utils.ImplTiming (ImplTiming(..))
import qualified Sql

data MispredictionStrategy
    = None
    | FirstInSet
    | Ranked Column Ranking

data PredictorConfig = PConfig
    { pConfigModelName :: Text
    , pConfigModelId :: Key PredictionModel
    , pConfigDefaultImpl :: Key Implementation
    , pConfigStrategy :: MispredictionStrategy
    }

getPredictorConfigAlgorithmId :: PredictorConfig -> SqlM (Key Algorithm)
getPredictorConfigAlgorithmId PConfig{..} = do
    predictionModelAlgorithmId <$> Sql.getJust pConfigModelId

getMispredictionStrategy
    :: Key PredictionModel
    -> MispredictionStrategy
    -> SqlM (Map Int64 UnknownSet -> Int -> Int)
getMispredictionStrategy _ None = return $ \_ _ -> (-1)

getMispredictionStrategy _ FirstInSet = return lookupMis
  where
    lookupMis :: Map Int64 UnknownSet -> Int -> Int
    lookupMis unknownPreds n = case M.lookup unknownId unknownPreds of
        Nothing -> -1
        Just UnknownSet{unknownSetImpls} ->
            fromIntegral . fromSqlKey $ S.findMin unknownSetImpls
      where
        unknownId = negate (fromIntegral n)

getMispredictionStrategy modelId (Ranked col rank) = do
    stepConfig <- getStepInfoConfig <$> getModelTrainingConfig modelId
    rankVector <- runSqlQuerySingle (implRankQuery stepConfig col rank)
    rankMap <- V.foldM' buildRankMap M.empty rankVector
    return $ lookupMis rankMap
  where
    lookupMis
        :: Map (Key Implementation) (Arg Double (Key Implementation))
        -> Map Int64 UnknownSet
        -> Int
        -> Int
    lookupMis implRank unknownSets n = fromMaybe (-1) $
        M.lookup unknownId predictionMap
      where
        unknownId = negate (fromIntegral n)
        predictionMap = M.mapMaybe (resolveUnknownSet implRank) unknownSets

resolveUnknownSet
    :: Map (Key Implementation) (Arg Double (Key Implementation))
    -> UnknownSet
    -> Maybe Int
resolveUnknownSet implRank UnknownSet{unknownSetImpls} =
    unwrap <$> foldMap (Just . Semi.Min) unknownSetRanks
  where
    unwrap :: Min (Arg Double (Key Implementation)) -> Int
    unwrap (Semi.Min (Semi.Arg _ impl)) = fromIntegral $ fromSqlKey impl

    unknownSetRanks = M.restrictKeys implRank unknownSetImpls

buildRankMap
    :: Map (Key Implementation) (Arg Double (Key Implementation))
    -> ImplTiming
    -> SqlM (Map (Key Implementation) (Arg Double (Key Implementation)))
buildRankMap implMap ImplTiming{..} = do
    key <- Sql.validateKey implTimingImpl

    when (key `M.member` implMap) . logThrowM $ GenericInvariantViolation
        "Found duplicate implementation id in input!"

    return $ M.insert key (Semi.Arg implTimingTiming key) implMap

mkPredictorConfig
    :: (Int64, Either Int Text, MispredictionStrategy)
    -> SqlM PredictorConfig
mkPredictorConfig (model, defImpl, strategy) = do
    Entity modelId PredictionModel{..} <-
        Sql.validateEntity model

    algorithm <- Sql.getJust predictionModelAlgorithmId
    impls <- Sql.queryImplementations predictionModelAlgorithmId

    let lookupByName :: Text -> Maybe Int
        lookupByName t = fmap fst
                        . listToMaybe
                        . filter ((t==) . implementationName . snd)
                        $ IM.toList impls

    defaultImpl <- fromIntegral <$> case defImpl of
        Left i | IM.member i impls -> return i
        Right t | Just i <- lookupByName t -> return i
        _ -> logThrowM $ UnexpectedMissingData
                "Default implementation not found for algorithm"
                (getAlgoName algorithm)

    defaultImplId <- Sql.validateKey defaultImpl

    return $ PConfig predictorName modelId defaultImplId strategy
  where
    predictorName :: Text
    predictorName = mconcat [showText model, ":", implName, ":", strategyName]

    implName :: Text
    implName = case defImpl of
        Left i -> showText i
        Right t -> t

    strategyName :: Text
    strategyName = case strategy of
        None -> "none"
        FirstInSet -> "firstinset"
        Ranked column rank -> renderColumn column <> "-" <> renderRank rank

    renderColumn col = case col of
        MinTime -> "min"
        AvgTime -> "avg"
        MaxTime -> "max"

    renderRank rank = case rank of
        Min -> "min"
        Avg -> "avg"
        Total -> "total"
