{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Exceptions where

import Control.Monad.Catch (Exception(..), MonadCatch, SomeException)
import qualified Control.Monad.Catch as Except
import Database.Sqlite (SqliteException(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)

mapException
    :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f = Except.handle (Except.throwM . f)

data BenchmarkException = forall e . Exception e => BenchmarkException e
    deriving (Typeable)

instance Show BenchmarkException where
    show (BenchmarkException e) = show e

instance Exception BenchmarkException where
    displayException (BenchmarkException e) = displayException e

toBenchmarkException :: Exception e => e -> SomeException
toBenchmarkException = toException . BenchmarkException

fromBenchmarkException :: Exception e => SomeException -> Maybe e
fromBenchmarkException exc = do
    BenchmarkException e <- fromException exc
    cast e

data ViolatedInvariant = forall e . Exception e => ViolatedInvariant e
    deriving (Typeable)

instance Show ViolatedInvariant where
    show (ViolatedInvariant e) = show e

instance Exception ViolatedInvariant where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException (ViolatedInvariant e) = displayException e

toViolatedInvariant :: Exception e => e -> SomeException
toViolatedInvariant = toException . ViolatedInvariant

fromViolatedInvariant :: Exception e => SomeException -> Maybe e
fromViolatedInvariant exc = do
    ViolatedInvariant e <- fromException exc
    cast e

{-
invariant violated by user input:
    Pattern match failure
    non-existent foreign key
    no default implementation for algorithm
    no elements in query dump
    -}

data RuntimeError = forall e . Exception e => RuntimeError e
    deriving (Typeable)

instance Show RuntimeError where
    show (RuntimeError e) = show e

instance Exception RuntimeError where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException (RuntimeError e) = displayException e

toRuntimeError :: Exception e => e -> SomeException
toRuntimeError = toException . RuntimeError

fromRuntimeError :: Exception e => SomeException -> Maybe e
fromRuntimeError exc = do
    RuntimeError e <- fromException exc
    cast e

{-
runtime problem:
    C++ not compiled / dead symlink
    srun job timed out
    srun job exited with error
    failed to run plot process
    -}

data ImpossibleError = forall e . Exception e => Impossible e
    deriving (Typeable)

instance Show ImpossibleError where
    show (Impossible e) = show e

instance Exception ImpossibleError where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException (Impossible e) = displayException e

toImpossibleError :: Exception e => e -> SomeException
toImpossibleError = toException . Impossible

fromImpossibleError :: Exception e => SomeException -> Maybe e
fromImpossibleError exc = do
    Impossible e <- fromException exc
    cast e

    {-
can't happen:
    query result conversion failure
    unexpected missing query results
    corrupted query output / implementation mismatch when plotting
    parse error while rendering query plan
    -}

data SchemaException = forall e . Exception e => SchemaException e
    deriving (Typeable)

instance Show SchemaException where
    show (SchemaException e) = show e

instance Exception SchemaException where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException (SchemaException e) = displayException e

toSchemaException :: Exception e => e -> SomeException
toSchemaException = toException . SchemaException

fromSchemaException :: Exception e => SomeException -> Maybe e
fromSchemaException exc = do
    SchemaException e <- fromException exc
    cast e

data SchemaWrong = WrongSchema
    deriving (Show, Typeable)

instance Exception SchemaWrong where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException WrongSchema = "Found wrong schema for schema version!"

data AbortMigration = AbortMigration
    deriving (Show, Typeable)

instance Exception AbortMigration where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException AbortMigration = "Aborted: Migration failed!"

data SqlException = forall e . Exception e => SqlException e
    deriving (Typeable)

instance Show SqlException where
    show (SqlException e) = show e

instance Exception SqlException where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException (SqlException e) = displayException e

toSqlException :: Exception e => e -> SomeException
toSqlException = toException . SqlException

fromSqlException :: Exception e => SomeException -> Maybe e
fromSqlException exc = do
    SqlException e <- fromException exc
    cast e

data ExpectedSingleValue = ExpectedSingleValue Text String
    deriving (Show, Typeable)

instance Exception ExpectedSingleValue where
    toException = toSqlException
    fromException = fromSqlException

    displayException (ExpectedSingleValue q v) = unlines
        [ "Query error for: " <> T.unpack q
        , "Expected a single value query result. Got: " <> v
        ]

newtype PrettySqliteException = PrettySqliteException SqliteException
    deriving (Show, Typeable)

instance Exception PrettySqliteException where
    toException = toSqlException
    fromException = fromSqlException
    displayException (PrettySqliteException SqliteException{..}) =
      T.unpack $ mconcat
        [ T.replace "\\\"" "\"" . T.replace "\\n" "\n" $ seFunctionName
        , "\n\n", T.pack (show seError), seDetails
        ]
