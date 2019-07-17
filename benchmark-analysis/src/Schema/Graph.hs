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
module Schema.Graph where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (EntityDef, Int64, MonadMigrate, (.=))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Graph
    name Text
    dataset Text
    path Text
    prettyName Text Maybe
    UniqGraph path
    UniqGraphName path dataset
    deriving Eq Show
|]

migrations :: MonadMigrate m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup schema
    [ 0 .= schema $ do
        Utils.executeMigrationSql [i|
ALTER TABLE 'Graph'
ADD COLUMN 'dataset' VARCHAR NOT NULL DEFAULT 'unknown'
|]
    ]
