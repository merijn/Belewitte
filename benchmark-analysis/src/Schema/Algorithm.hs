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
import qualified Database.Persist.TH as TH

import Pretty.Fields
import Schema.Utils (EntityDef, Int64, MonadSql, Transaction, (.=))
import qualified Schema.Utils as Utils

TH.share [TH.mkPersist TH.sqlSettings, TH.mkSave "schema"] [persistUpperCase|
Algorithm
    name Text
    prettyName Text Maybe
    UniqAlgorithm name
    deriving Eq Show
|]

deriving instance Show (Unique Algorithm)

instance PrettyFields Algorithm where
    prettyFieldInfo = ("Id", idField AlgorithmId) :|
        [ ("Name", textField AlgorithmName)
        , ("Pretty Name", maybeTextField AlgorithmPrettyName)
        ]

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup [ 0 .= schema ]
