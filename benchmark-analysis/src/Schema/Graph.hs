{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.>), (.=))
import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import qualified Schema.Graph.V0 as V0

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Graph
    name Text
    path Text
    prettyName Text Maybe
    datasetId DatasetId
    UniqGraph path
    UniqGraphName name datasetId
    deriving Eq Show
|]

instance PrettyColumns Graph where
    prettyColumnInfo = idColumn GraphId :|
        [ column GraphName
        , idColumn GraphDatasetId
        , maybeColumn GraphPrettyName
        , column GraphPath
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE 'Graph'
ADD COLUMN 'dataset' VARCHAR NOT NULL DEFAULT 'unknown'
|]
    , 5 .= schema
    ]
