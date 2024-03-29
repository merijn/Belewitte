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
module Schema.Platform.V0 where

import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [Utils.mkSchema|
GPU
    name Text
    prettyName Text Maybe
    UniqGPU name
    deriving Eq Show
|]
