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
module Schema.Implementation where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.=))
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

instance PrettyColumns Implementation where
    prettyColumnInfo = ("Id", idColumn ImplementationId) :|
        [ ("Algorithm", idColumn ImplementationAlgorithmId)
        , ("Name", column ImplementationName)
        , ("Type", ImplementationType `columnVia` prettyShow)
        , ("Pretty Name", maybeColumn ImplementationPrettyName)
        , ("Flags", maybeColumn ImplementationFlags)
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
