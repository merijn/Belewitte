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

import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Platform
    name Text
    prettyName Text Maybe
    UniqPlatform name
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE 'GPU' RENAME TO 'Platform'
|]
    ]
