{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema.Timers where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Database.Persist.Sql as Sql
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Schema.Utils (Int64, MigrationAction, (.=), mkMigrationLookup)

import Schema.Implementation (ImplementationId)
import Schema.Platform (PlatformId)
import Schema.Variant (VariantId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
TotalTimer
    platformId PlatformId
    variantId VariantId
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime
    Primary platformId variantId implId name
    deriving Eq Show

StepTimer
    platformId PlatformId
    variantId VariantId
    stepId Int
    implId ImplementationId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    timestamp UTCTime
    Primary platformId variantId stepId implId name
    deriving Eq Show
|]

migrations :: Int64 -> MigrationAction
migrations = mkMigrationLookup schema
    [ 0 .= schema $ do
        Sql.rawExecute [i|
ALTER TABLE 'TotalTimer' RENAME COLUMN 'gpuId' TO 'platformId'
|] []
        Sql.rawExecute [i|
ALTER TABLE 'StepTimer' RENAME COLUMN 'gpuId' TO 'platformId'
|] []
    ]
