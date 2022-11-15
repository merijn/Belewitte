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
module Schema.Algorithm where

import Data.Text (Text)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Pretty.Fields.Persistent
import Schema.Import (Importable(updateFields))
import Schema.Utils (Entity, EntityDef, Int64, MonadSql, Transaction, (.=))
import qualified Schema.Utils as Utils

Utils.mkEntities "schema" [persistUpperCase|
Algorithm
    name Text
    prettyName Text Maybe
    UniqAlgorithm name
    deriving Eq Show
|]

deriving instance Show (Unique Algorithm)

instance PrettyFields (Entity Algorithm) where
    prettyFieldInfo = ("Id", idField AlgorithmId) :|
        [ ("Name", textField AlgorithmName)
        , ("Pretty Name", maybeTextField AlgorithmPrettyName)
        ]

instance NamedEntity Algorithm where
    entityName = optionalPrettyName algorithmPrettyName algorithmName

instance Importable Algorithm where
    updateFields = []

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
