{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
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
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.>), (.=))
import qualified Schema.Utils as Utils

import qualified Schema.Platform.V0 as V0

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
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'GPU' RENAME TO 'Platform'
|]
    , 10 .= schema
    ]
