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
module Schema.Model.V0 where

import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql

import Model (Model)
import qualified Schema.Utils as Utils

import Schema.Platform.V0 (GPUId)
import qualified Schema.Platform.V0 as Platform

Utils.mkEntitiesWith "schema" [Platform.schema] [Utils.mkSchema|
PredictionModel
    gpuId GPUId
    model Model
    trainFraction Double default=0
    trainSeed Int default=42
    totalUnknownCount Int
    timestamp UTCTime default="(strftime('%Y-%m-%dT%H:%M:%f',1,'unixepoch'))"
|]
