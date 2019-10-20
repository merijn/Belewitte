{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Dataset where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Dataset
    name Text
    UniqDataset name
    deriving Eq Show
|]

instance PrettyFields Dataset where
    prettyFieldInfo = ("Id", idField DatasetId) :|
        [ ("Name", textField DatasetName) ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 5 .> schema $ do
        Utils.createTableFromSchema schema

        Utils.executeSql [i|
INSERT INTO "Dataset"
SELECT ROW_NUMBER() OVER (ORDER BY dataset), dataset
FROM (SELECT DISTINCT dataset FROM Graph)
|]

        Utils.executeSql [i|
ALTER TABLE "Graph"
ADD COLUMN "datasetId" INTEGER REFERENCES "Dataset"
|]

        Utils.executeSql [i|
UPDATE "Graph"
SET "datasetId" = (SELECT "id" FROM "Dataset" WHERE "name" = "dataset")
|]
    ]
