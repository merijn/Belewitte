{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor.Config
    ( Column(..)
    , MispredictionStrategy(..)
    , PredictorConfig(..)
    , Ranking(..)
    , getMispredictionStrategy
    ) where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
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
    { pConfigModelId :: Key PredictionModel
    , pConfigDefaultImpl :: Key Implementation
    , pConfigStrategy :: MispredictionStrategy
    }

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
    key <- Sql.validateKey "Implementation" implTimingImpl

    when (key `M.member` implMap) . logThrowM $ GenericInvariantViolation
        "Found duplicate implementation id in input!"

    return $ M.insert key (Semi.Arg implTimingTiming key) implMap
