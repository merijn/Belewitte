{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Schema.Import (ImportType(..), Importable(..), UpdateField(..)) where

import Database.Persist.Class (SafeToInsert)

import Sql.Core

data UpdateField rec where
    ForeignKeyField
        :: Importable r => EntityField rec (Key r) -> UpdateField rec

    ForeignKeyFieldAllowMissing
        :: Importable r => EntityField rec (Key r) -> UpdateField rec

data ImportType rec where
    PrimaryImport :: ImportType rec
    UniqueImport
        :: (AtLeastOneUniqueKey rec, SafeToInsert rec, Show (Unique rec))
        => ImportType rec

    ExplicitUniqueImport
        :: (AtLeastOneUniqueKey rec, SafeToInsert rec, Show (Unique rec))
        => (Unique rec -> Bool) -> ImportType rec

class (Eq v, Show v, SqlRecord v) => Importable v where
    updateFields :: [UpdateField v]

    importType :: v -> ImportType v
    default importType
        :: (AtLeastOneUniqueKey v, SafeToInsert v, Show (Unique v))
        => v -> ImportType v
    importType _ = UniqueImport
