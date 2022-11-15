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
import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils

import Schema.Algorithm (AlgorithmId)
import Schema.Dataset (DatasetId)
import Schema.Platform (PlatformId)

Utils.mkEntities "schema" [persistUpperCase|
RunConfig
    algorithmId AlgorithmId
    platformId PlatformId
    datasetId DatasetId
    algorithmVersion Text
    repeats Int
    deriving Eq Show
|]
