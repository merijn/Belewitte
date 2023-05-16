{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.GlobalVars where

import Data.Text (Text)
import Database.Persist.Sql (Unique)

import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.=))
import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [Utils.mkSchema|
GlobalVars
    name Text
    value Text
    Primary name
    UniqGlobal name
    deriving Eq Show
|]

deriving instance Show (Unique GlobalVars)

data GlobalVar a where
    RunCommand :: GlobalVar Text

deriving instance Show (GlobalVar a)

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup [ 11 .= schema ]
