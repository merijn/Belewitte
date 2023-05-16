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
module Schema.Graph.V2 where

import Data.Text (Text)

import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import qualified Schema.Utils as Utils

Utils.mkEntitiesWith "schema" [Dataset.schema] [Utils.mkSchema|
Graph
    name Text
    path Text
    prettyName Text Maybe
    datasetId DatasetId
    UniqGraph path
    UniqGraphName name datasetId
    deriving Eq Show
|]
