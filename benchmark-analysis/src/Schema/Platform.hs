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
{-# LANGUAGE UndecidableInstances #-}
module Schema.Platform where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>), (.=))
import qualified Schema.Utils as Utils

import qualified Schema.Platform.V0 as V0
import qualified Schema.Platform.V1 as V1

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Platform
    name Text
    prettyName Text Maybe
    flags Text Maybe
    available Int default=1
    isDefault Bool default=0
    UniqPlatform name
    deriving Eq Show
|]

deriving instance Show (Unique Platform)

instance PrettyFields Platform where
    prettyFieldInfo = ("Id", idField PlatformId) :|
        [ ("Name", textField PlatformName)
        , ("Pretty Name", maybeTextField PlatformPrettyName)
        , ("Flags", maybeTextField PlatformFlags)
        , ("Num. Available", PlatformAvailable `fieldVia` prettyShow)
        , ("Default Platform", PlatformIsDefault `fieldVia` prettyShow)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 1 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'GPU' RENAME TO 'Platform'
|]
    , 10 .= schema
    ]
