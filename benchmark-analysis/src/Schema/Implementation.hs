{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.=))
import qualified Schema.Utils as Utils
import Types

import Schema.Algorithm (AlgorithmId)

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Implementation
    algorithmId AlgorithmId
    name Text
    prettyName Text Maybe
    flags Text Maybe
    type ImplType
    UniqImpl algorithmId name
    deriving Eq Show
|]

instance PrettyFields Implementation where
    prettyFieldInfo = ("Id", idField ImplementationId) :|
        [ ("Algorithm", idField ImplementationAlgorithmId)
        , ("Name", textField ImplementationName)
        , ("Type", ImplementationType `fieldVia` prettyShow)
        , ("Pretty Name", maybeTextField ImplementationPrettyName)
        , ("Flags", maybeTextField ImplementationFlags)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
