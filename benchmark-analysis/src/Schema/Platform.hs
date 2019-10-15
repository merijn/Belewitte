{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Platform where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.>), (.=))
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

instance PrettyColumns Platform where
    prettyColumnInfo = idColumn PlatformId :|
        [ column PlatformName
        , maybeColumn PlatformPrettyName
        , maybeColumn PlatformFlags
        , PlatformAvailable `columnVia` prettyShow
        , PlatformIsDefault `columnVia` prettyShow
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'GPU' RENAME TO 'Platform'
|]
    , 10 .= schema
    ]
