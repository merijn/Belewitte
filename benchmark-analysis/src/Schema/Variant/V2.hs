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
{-# LANGUAGE UndecidableInstances #-}
module Schema.Variant.V2 where

import Database.Persist.TH (persistUpperCase)

import Schema.Utils (EntityDef, ForeignDef)
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Graph (GraphId)
import Schema.VariantConfig (VariantConfigId)

Utils.mkEntities "schema'" [persistUpperCase|
Variant
    graphId GraphId
    variantConfigId VariantConfigId
    algorithmId AlgorithmId
    result Hash Maybe
    propsStored Bool
    retryCount Int
    UniqVariant graphId variantConfigId
    deriving Eq Show
|]

schema :: [EntityDef]
schema = Utils.addForeignRef "Variant" variantConfig schema'
  where
    variantConfig :: ForeignDef
    variantConfig = Utils.mkForeignRef "VariantConfig"
        [ ("variantConfigId", "id"), ("algorithmId", "algorithmId") ]
