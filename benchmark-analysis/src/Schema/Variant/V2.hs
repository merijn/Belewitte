{-# LANGUAGE DataKinds #-}
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
module Schema.Variant.V2 where

import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Graph (GraphId)
import qualified Schema.Graph as Graph
import Schema.VariantConfig (VariantConfigId)
import qualified Schema.VariantConfig as VariantConfig

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Graph.schema, VariantConfig.schema] [Utils.mkSchema|
Variant
    graphId GraphId
    variantConfigId VariantConfigId
    algorithmId AlgorithmId
    result Hash Maybe
    propsStored Bool
    retryCount Int
    UniqVariant graphId variantConfigId
    Foreign VariantConfig foreignVariantConfig variantConfigId algorithmId References Id algorithmId
    deriving Eq Show
|]
