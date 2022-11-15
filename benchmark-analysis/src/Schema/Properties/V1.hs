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
{-# LANGUAGE UndecidableInstances #-}
module Schema.Properties.V1 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)

import Schema.Utils (EntityDef, ForeignDef)
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.Variant (VariantId)

Utils.mkEntities "schema'" [persistUpperCase|
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
    deriving Eq Show
|]

schema :: [EntityDef]
schema = Utils.addForeignRef "StepPropValue" stepProp
       . Utils.addForeignRef "StepPropValue" variant
       $ schema'
  where
    stepProp :: ForeignDef
    stepProp = Utils.mkForeignRef "StepProp"
        [ ("algorithmId", "algorithmId"), ("property", "property") ]

    variant :: ForeignDef
    variant = Utils.mkForeignRef "Variant"
        [ ("variantId", "id"), ("algorithmId", "algorithmId") ]
