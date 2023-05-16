{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties.V1 where

import Data.Text (Text)

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Graph (GraphId)
import qualified Schema.Graph as Graph
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Graph.schema, Variant.schema] [Utils.mkSchema|
GraphProp
    graphId GraphId
    property Text
    value Double
    Primary graphId property
    UniqGraphProp graphId property
    deriving Eq Show

StepProp
    algorithmId AlgorithmId
    property Text
    Primary algorithmId property
    UniqStepProp algorithmId property
    deriving Eq Show

StepPropValue
    algorithmId AlgorithmId
    variantId VariantId
    stepId Int
    property Text
    value Double
    Primary variantId stepId property
    UniqStepPropValue variantId stepId property
    Foreign StepProp foreignStepProp algorithmId property References algorithmId property
    Foreign Variant foreignVariant variantId algorithmId References Id algorithmId
    deriving Eq Show
|]
