{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Timers.V0 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Implementation (ImplementationId)
import Schema.Platform (PlatformId)
import Schema.Variant (VariantId)
import Types

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
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
