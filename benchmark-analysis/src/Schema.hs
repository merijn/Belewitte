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
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.TH

import Model (Model)
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "src/schema")
