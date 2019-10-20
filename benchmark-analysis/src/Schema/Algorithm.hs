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
module Schema.Algorithm where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)
import qualified Database.Persist.TH as TH

import Pretty.Columns
import Schema.Utils (EntityDef, Int64, MonadSql, (.=))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Algorithm
    name Text
    prettyName Text Maybe
    UniqAlgorithm name
    deriving Eq Show
|]

instance PrettyColumns Algorithm where
    prettyColumnInfo = ("Id", idColumn AlgorithmId) :|
        [ ("Name", column AlgorithmName)
        , ("Pretty Name", maybeColumn AlgorithmPrettyName)
        ]

migrations :: MonadSql m => Int64 -> m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
