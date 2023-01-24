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
module Schema.UnknownPredictions.V2 where

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Model (PredictionModelId)
import qualified Schema.Model as Model
import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Model.schema, Implementation.schema] [Utils.mkSchema|
UnknownPrediction
    modelId PredictionModelId
    algorithmId AlgorithmId
    count Int
    Foreign PredictionModel foreignPredictionModel modelId algorithmId References Id algorithmId
    deriving Eq Show

UnknownPredictionSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    algorithmId AlgorithmId
    Primary unknownPredId implId
    Foreign UnknownPrediction foreignUnknownPrediction unknownPredId algorithmId References Id algorithmId
    deriving Eq Show
|]
