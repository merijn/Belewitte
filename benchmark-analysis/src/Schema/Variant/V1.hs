{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Variant.V1 where

import qualified Schema.Utils as Utils
import Types

import Schema.Graph (GraphId)
import qualified Schema.Graph as Graph
import Schema.VariantConfig (VariantConfigId)
import qualified Schema.VariantConfig as VariantConfig

Utils.mkEntitiesWith "schema"
    [Graph.schema, VariantConfig.schema] [Utils.mkSchema|
Variant
    graphId GraphId
    variantConfigId VariantConfigId
    result Hash Maybe
    propsStored Bool
    retryCount Int
    UniqVariant graphId variantConfigId
    deriving Eq Show
|]
