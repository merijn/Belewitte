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
import Database.Persist.Sql (Checkmark, Unique)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Import
import Schema.Utils
    (Entity, EntityDef, Int64, MonadSql, Transaction, (.>), (.=))
import qualified Schema.Utils as Utils

import qualified Schema.Platform.V0 as V0
import qualified Schema.Platform.V1 as V1
import qualified Schema.Platform.V2 as V2

Utils.mkEntities "schema" [persistUpperCase|
Platform
    name Text
    prettyName Text Maybe
    flags Text Maybe
    available Int default=1
    isDefault Checkmark nullable
    UniqPlatform name
    UniqPlatformDefault name isDefault !force
    deriving Eq Show
|]

deriving instance Show (Unique Platform)

instance PrettyFields (Entity Platform) where
    prettyFieldInfo = ("Id", idField PlatformId) :|
        [ ("Name", textField PlatformName)
        , ("Pretty Name", maybeTextField PlatformPrettyName)
        , ("Flags", maybeTextField PlatformFlags)
        , ("Num. Available", PlatformAvailable `fieldVia` prettyShow)
        , ("Default Platform", PlatformIsDefault `fieldVia` prettyShow)
        ]

instance NamedEntity Platform where
    entityName = optionalPrettyName platformPrettyName platformName

instance Importable Platform where
    updateFields = []

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 1 .> V1.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'GPU' RENAME TO 'Platform'
|]
    , 10 .= V2.schema
    , 29 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Platform"
RENAME COLUMN "isDefault" TO "oldIsDefault"
|]

        Utils.executeSql [i|
ALTER TABLE "Platform"
ADD COLUMN "isDefault" BOOLEAN
|]

        Utils.executeSql [i|
UPDATE "Platform"
SET "isDefault" =
  CASE WHEN "oldIsDefault" THEN 1
    ELSE NULL
  END
|]
    ]
