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
module Schema.Graph.V0 where

import Data.Text (Text)
import qualified Database.Persist.Sql as Sql

import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [Utils.mkSchema|
Graph
    name Text
    path Text
    prettyName Text Maybe
    UniqGraph path
    deriving Eq Show
|]
