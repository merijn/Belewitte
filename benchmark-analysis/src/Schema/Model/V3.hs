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
module Schema.Model.V3 where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Model (Model)
import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Platform.schema] [Utils.mkSchema|
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
