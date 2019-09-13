{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Schema.Utils (EntityDef, Int64, MonadSql, (.>))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Dataset
    name Text
    UniqDataset name
    deriving Eq Show
|]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 5 .> schema $ do
        Utils.executeSql [i|
CREATE TABLE IF NOT EXISTS "Dataset"
("id" INTEGER PRIMARY KEY
,"name" VARCHAR NOT NULL
,CONSTRAINT "UniqDataset" UNIQUE ("name")
)
|]

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
