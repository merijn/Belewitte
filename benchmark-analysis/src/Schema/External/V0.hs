{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Schema.External.V0 where

import Data.Text (Text)

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Platform.schema, Variant.schema] [Utils.mkSchema|
ExternalImpl
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    UniqExternalImpl algorithmId name
    deriving Eq Show

ExternalTimer
    platformId PlatformId
    variantId VariantId
    implId ExternalImplId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary platformId variantId implId name
    deriving Eq Show
|]
