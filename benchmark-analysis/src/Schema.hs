{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Schema
    ( ByteString
    , Model
    , Text
    , ImplType(..)
    , Hash(..)
    , module Schema
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Quasi
import Database.Persist.TH

import Model (Model)
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "src/schema")

getImplName :: Implementation -> Text
getImplName (Implementation _ name prettyName _ _ _) =
  fromMaybe name prettyName

