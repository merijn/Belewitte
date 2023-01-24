{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.UnknownPredictions.V0 where

import qualified Schema.Utils as Utils

import Schema.Model (PredictionModelId)
import qualified Schema.Model as Model
import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation

Utils.mkEntitiesWith "schema"
    [Model.schema, Implementation.schema] [Utils.mkSchema|
UnknownPrediction
    modelId PredictionModelId
    count Int
    deriving Eq Show

UnknownSet
    unknownPredId UnknownPredictionId
    implId ImplementationId
    Primary unknownPredId implId
    deriving Eq Show
|]
