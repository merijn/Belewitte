{-# LANGUAGE NamedFieldPuns #-}
module Predictor.Config
    ( MispredictionStrategy(..)
    , PredictorConfig(..)
    , getMispredictionStrategy
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Core
import Model.Stats (UnknownSet(..))
import Schema

data MispredictionStrategy
    = None
    | FirstInSet

data PredictorConfig = PConfig
    { pConfigModelId :: Key PredictionModel
    , pConfigDefaultImpl :: Key Implementation
    , pConfigStrategy :: MispredictionStrategy
    }

getMispredictionStrategy
    :: MispredictionStrategy -> SqlM (Map Int64 UnknownSet -> Int -> Int)
getMispredictionStrategy None = return $ \_ _ -> (-1)

getMispredictionStrategy FirstInSet = return lookupMis
  where
    lookupMis :: Map Int64 UnknownSet -> Int -> Int
    lookupMis unknownPreds n = case M.lookup unknownId unknownPreds of
        Nothing -> -1
        Just UnknownSet{unknownSetImpls} ->
            fromIntegral . fromSqlKey $ S.findMin unknownSetImpls
      where
        unknownId = negate (fromIntegral n)
