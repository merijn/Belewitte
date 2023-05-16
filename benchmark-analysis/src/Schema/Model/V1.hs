{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Schema.Model.V1 where

import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql

import Model (Model)
import qualified Schema.Utils as Utils

import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform

Utils.mkEntitiesWith "schema" [Platform.schema] [Utils.mkSchema|
PredictionModel
    platformId PlatformId
    model Model
    trainFraction Double
    trainSeed Int
    totalUnknownCount Int
    timestamp UTCTime
|]
