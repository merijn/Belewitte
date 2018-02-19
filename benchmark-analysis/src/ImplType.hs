{-# LANGUAGE TemplateHaskell #-}
module ImplType where

import Database.Persist.TH

data ImplType = Core | Derived | Comparison
    deriving (Show, Read, Eq)
derivePersistField "ImplType"
