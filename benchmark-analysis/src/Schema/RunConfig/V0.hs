{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.RunConfig.V0 where

import Data.Text (Text)

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Algorithm as Algorithm
import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import Schema.Platform (PlatformId)
import qualified Schema.Platform as Platform

Utils.mkEntitiesWith "schema"
    [Algorithm.schema, Dataset.schema, Platform.schema] [Utils.mkSchema|
RunConfig
    algorithmId AlgorithmId
    platformId PlatformId
    datasetId DatasetId
    algorithmVersion Text
    repeats Int
    deriving Eq Show
|]
