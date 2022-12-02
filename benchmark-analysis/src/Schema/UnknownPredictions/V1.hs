{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.UnknownPredictions.V1 where

import Database.Persist.TH (persistUpperCase)

import Schema.Utils (EntityDef, ForeignDef)
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Model (PredictionModelId)
import qualified Schema.Model as Model
import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation

Utils.mkEntitiesWith "schema'"
    [Algorithm.schema, Model.schema, Implementation.schema] [persistUpperCase|
UnknownPrediction
    modelId PredictionModelId
    algorithmId AlgorithmId
    count Int
    deriving Eq Show

UnknownSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    algorithmId AlgorithmId
    Primary unknownPredId implId
    deriving Eq Show
|]

schema :: [EntityDef]
schema = Utils.addForeignRef "UnknownPrediction" model
       . Utils.addForeignRef "UnknownSet" unknownPred
       $ schema'
  where
    model :: ForeignDef
    model = Utils.mkForeignRef "PredictionModel"
        [ ("modelId", "id"), ("algorithmId", "algorithmId") ]

    unknownPred :: ForeignDef
    unknownPred = Utils.mkForeignRef "UnknownPrediction"
        [ ("unknownPredId", "id"), ("algorithmId", "algorithmId") ]
