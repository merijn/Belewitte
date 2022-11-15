{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Dataset where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Import
import Schema.Utils (Entity, EntityDef, Int64, MonadSql, Transaction, (.>))
import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [persistUpperCase|
Dataset
    name Text
    UniqDataset name
    deriving Eq Show
|]

deriving instance Show (Unique Dataset)

instance PrettyFields (Entity Dataset) where
    prettyFieldInfo = ("Id", idField DatasetId) :|
        [ ("Name", textField DatasetName) ]

instance NamedEntity Dataset where
    entityName = datasetName

instance Importable Dataset where
    updateFields = []

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
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
