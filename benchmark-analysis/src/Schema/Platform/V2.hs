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
module Schema.Platform.V2 where

import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [Utils.mkSchema|
Platform
    name Text
    prettyName Text Maybe
    flags Text Maybe
    available Int default=1
    isDefault Bool default=0
    UniqPlatform name
    deriving Eq Show
|]
