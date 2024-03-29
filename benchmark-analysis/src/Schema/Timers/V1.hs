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
module Schema.Timers.V1 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import qualified Schema.Utils as Utils
import Types

import Schema.Implementation (ImplementationId)
import qualified Schema.Implementation as Implementation
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform
import Schema.Variant (VariantId)
import qualified Schema.Variant as Variant

Utils.mkEntitiesWith "schema"
    [Implementation.schema, Platform.schema, Variant.schema] [Utils.mkSchema|
TotalTimer
    platformId PlatformId
    variantId VariantId
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime
    wrongResult Hash Maybe
    Primary platformId variantId implId name
    deriving Eq Show

StepTimer
    platformId PlatformId
    variantId VariantId
    stepId Int
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime
    wrongResult Hash Maybe
    Primary platformId variantId stepId implId name
    deriving Eq Show
|]
