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
{-# LANGUAGE UndecidableInstances #-}
module Schema.Timers.V0 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils
import Types

import Schema.Implementation (ImplementationId)
import Schema.Platform.V0 (GPUId)
import Schema.Variant (VariantId)

Utils.mkEntities "schema" [persistUpperCase|
TotalTimer
    gpuId GPUId
    variantId VariantId
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime default="(strftime('%Y-%m-%dT%H:%M:%f',0,'unixepoch'))"
    wrongResult Hash Maybe
    Primary gpuId variantId implId name
    deriving Eq Show

StepTimer
    gpuId GPUId
    variantId VariantId
    stepId Int
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime default="(strftime('%Y-%m-%dT%H:%M:%f',0,'unixepoch'))"
    wrongResult Hash Maybe
    Primary gpuId variantId stepId implId name
    deriving Eq Show
|]
