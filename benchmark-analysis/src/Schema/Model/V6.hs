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
{-# LANGUAGE UndecidableInstances #-}
module Schema.Model.V6 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (persistUpperCase)

import Model (Model)
import Schema.Utils (Int64)
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import Schema.Platform (PlatformId)

Utils.mkEntities "schema" [persistUpperCase|
PredictionModel
    platformId PlatformId
    algorithmId AlgorithmId
    algorithmVersion CommitId
    name Text
    prettyName Text Maybe
    description Text Maybe
    model Model
    skipIncomplete Bool
    legacyTrainFraction Double
    trainGraphs Percentage
    trainVariants Percentage
    trainSteps Percentage
    trainSeed Int64
    totalUnknownCount Int
    timestamp UTCTime
    UniqModel name
|]
