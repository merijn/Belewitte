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
module Schema.Implementation where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Utils
    (Entity, EntityDef, Int64, MonadSql, Transaction, (.=), (.>))
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)
import qualified Schema.Implementation.V0 as V0

Utils.mkEntities "schema" [persistUpperCase|
Implementation
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    flags Text Maybe
    type ImplType
    timestamp UTCTime
    UniqImpl algorithmId name
    deriving Eq Show
|]

deriving instance Show (Unique Implementation)

instance PrettyFields (Entity Implementation) where
    prettyFieldInfo = ("Id", idField ImplementationId) :|
        [ ("Algorithm", namedIdField ImplementationAlgorithmId)
        , ("Name", textField ImplementationName)
        , ("Type", ImplementationType `fieldVia` prettyShow)
        , ("Pretty Name", maybeTextField ImplementationPrettyName)
        , ("Flags", maybeTextField ImplementationFlags)
        , ("Timestamp", ImplementationTimestamp `fieldVia` prettyShow)
        ]

instance NamedEntity Implementation where
    entityName = optionalPrettyName implementationPrettyName implementationName

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 0 .= V0.schema
    , 26 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Implementation" ADD COLUMN "timestamp" TIMESTAMP
|]

        Utils.executeSql [i|
REPLACE INTO "Implementation"
SELECT Implementation.id
     , Implementation.algorithmId
     , Implementation.name
     , Implementation.prettyName
     , Implementation.flags
     , Implementation.type
     , IFNULL(TimeStamp.minTime, strftime('%Y-%m-%dT%H:%M:%f', 'now'))
FROM Implementation
LEFT JOIN
(   SELECT Run.implId, MIN(Run.timestamp) AS minTime
    FROM Run
    GROUP BY Run.implId
) AS TimeStamp
ON Implementation.id = TimeStamp.implId
|]
    ]
