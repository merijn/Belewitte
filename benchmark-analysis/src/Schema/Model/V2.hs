{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Model.V2 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Model (Model)

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
PredictionModel
    platformId PlatformId
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    trainFraction Double
    trainSeed Int
    totalUnknownCount Int
    timestamp UTCTime
    UniqModel name
|]
