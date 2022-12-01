{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Exceptions
    ( Error(..)
    , Exception(..)
    , MonadThrow
    , Except.MonadCatch
    , Except.MonadMask
    , Pretty(pretty)
    , SomeException(..)
    , SqlType(..)
    , SqliteException(..)
    , module Exceptions
    , module Exceptions.Class
    ) where

import Control.Monad.Catch (Exception(..), MonadThrow, SomeException)
import qualified Control.Monad.Catch as Except
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (UnicodeException(..))
import Data.Typeable (Typeable, cast)
import Database.Persist.Class (Unique)
import Database.Persist.Types (SqlType(..), PersistValue(..))
import Database.Sqlite (Error(..), SqliteException(..))

import Exceptions.Class
import Pretty (Doc, Pretty(pretty), (<+>))
import qualified Pretty

renderList :: Pretty e => [e] -> Doc a
renderList = renderPrettyList . map pretty

renderPrettyList :: [Doc a] -> Doc a
renderPrettyList = Pretty.align . Pretty.encloseSep "[" "]" ", "

data MissingPrimaryKey = MissingPrimaryKey Text
    deriving (Show, Typeable)

instance Pretty MissingPrimaryKey where
    pretty (MissingPrimaryKey name) = Pretty.reflow $ mconcat
        [ "Missing primary key definition for '", name, "'" ]

instance Exception MissingPrimaryKey where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

data ViolatedInvariant where
    ViolatedInvariant :: (Exception e, Pretty e) => e -> ViolatedInvariant
    deriving (Typeable)

instance Pretty ViolatedInvariant where
    pretty (ViolatedInvariant e) = pretty e

instance Show ViolatedInvariant where
    show (ViolatedInvariant e) = show e

instance Exception ViolatedInvariant where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

toViolatedInvariant :: (Exception e, Pretty e) => e -> SomeException
toViolatedInvariant = toException . ViolatedInvariant

fromViolatedInvariant :: (Exception e, Pretty e) => SomeException -> Maybe e
fromViolatedInvariant exc = do
    ViolatedInvariant e <- fromException exc
    cast e

data PatternFailed = PatternFailed Text
    deriving (Show, Typeable)

instance Pretty PatternFailed where
    pretty (PatternFailed txt) = Pretty.group $ mconcat
        [ Pretty.reflow "A database lookup failed due to erroneous user input:"
        , Pretty.line
        , Pretty.reflow txt
        , Pretty.hardline
        , Pretty.reflow "Did you specify the right argument and/or database?"
        ]

instance Exception PatternFailed where
    toException = toViolatedInvariant
    fromException = fromViolatedInvariant
    displayException = show . pretty

data UnexpectedMissingData = UnexpectedMissingData Text Text
    deriving (Show, Typeable)

instance Pretty UnexpectedMissingData where
    pretty (UnexpectedMissingData msg name) = mconcat
        [ Pretty.reflow msg, Pretty.line, Pretty.dquotes (pretty name) ]

instance Exception UnexpectedMissingData where
    toException = toViolatedInvariant
    fromException = fromViolatedInvariant
    displayException = show . pretty

data GenericInvariantViolation = GenericInvariantViolation Text
    deriving (Show, Typeable)

instance Pretty GenericInvariantViolation where
    pretty (GenericInvariantViolation txt) = Pretty.reflow txt <> Pretty.line

instance Exception GenericInvariantViolation where
    toException = toViolatedInvariant
    fromException = fromViolatedInvariant
    displayException = show . pretty

data DBConstraintViolation = DBConstraintViolation Text
    deriving (Show, Typeable)

instance Pretty DBConstraintViolation where
    pretty (DBConstraintViolation txt) = Pretty.group $ mconcat
        [ Pretty.reflow "Database constraint violated:", Pretty.line
        , Pretty.reflow txt, Pretty.hardline
        ]

instance Exception DBConstraintViolation where
    toException = toViolatedInvariant
    fromException = fromViolatedInvariant
    displayException = show . pretty

data RuntimeError where
    RuntimeError :: (Exception e, Pretty e) => e -> RuntimeError
    deriving (Typeable)

instance Pretty RuntimeError where
    pretty (RuntimeError e) = pretty e

instance Show RuntimeError where
    show (RuntimeError e) = show e

instance Exception RuntimeError where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

toRuntimeError :: (Exception e, Pretty e) => e -> SomeException
toRuntimeError = toException . RuntimeError

fromRuntimeError :: (Exception e, Pretty e) => SomeException -> Maybe e
fromRuntimeError exc = do
    RuntimeError e <- fromException exc
    cast e

data MissingCxxKernelRunner = MissingCxxKernelRunner
    deriving (Show, Typeable)

instance Pretty MissingCxxKernelRunner where
    pretty MissingCxxKernelRunner = Pretty.reflow
        "Kernel runner executable does not exist. \
        \Perhaps the C++ code was not compiled?"

instance Exception MissingCxxKernelRunner where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data MissingKernelLibPath = MissingKernelLibPath
    deriving (Show, Typeable)

instance Pretty MissingKernelLibPath where
    pretty MissingKernelLibPath = Pretty.reflow
        "Kernel directory non-existent. \
        \Perhaps CUDA kernels were not compiled?"

instance Exception MissingKernelLibPath where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data MissingPython = MissingPython
    deriving (Show, Typeable)

instance Pretty MissingPython where
    pretty MissingPython = Pretty.reflow
        "Unable to locate 'python3' interpreter!"

instance Exception MissingPython where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data StdinDisappeared = StdinDisappeared
    deriving (Show, Typeable)

instance Pretty StdinDisappeared where
    pretty StdinDisappeared = Pretty.reflow
        "stdin was unexpectedly closed while waiting for user input!"

instance Exception StdinDisappeared where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data MissingEntity = MissingEntity Text Int64
    deriving (Show, Typeable)

instance Pretty MissingEntity where
    pretty (MissingEntity name k) = Pretty.reflow $
        "Missing " <> name <> " with id #" <> T.pack (show k)

instance Exception MissingEntity where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data MissingUniqEntity where
    MissingUniqEntity
        :: Show (Unique v) => Text -> (Unique v) -> MissingUniqEntity
    deriving (Typeable)

instance Show MissingUniqEntity where
    show (MissingUniqEntity name uniq) =
        "MissingUniqEntity " <> T.unpack name <> " " <> show uniq

instance Pretty MissingUniqEntity where
    pretty (MissingUniqEntity name uniq) = Pretty.reflow $
        "Missing " <> name <> " for " <> T.pack (show uniq)

instance Exception MissingUniqEntity where
    toException = toRuntimeError
    fromException = fromRuntimeError
    displayException = show . pretty

data ImpossibleError where
    Impossible :: (Exception e, Pretty e) => e -> ImpossibleError
    deriving (Typeable)

instance Pretty ImpossibleError where
    pretty (Impossible e) = mconcat
        [ pretty e, Pretty.hardline, Pretty.hardline, Pretty.reflow msg ]
      where
        msg = "This should be impossible, please file a bug report."

instance Show ImpossibleError where
    show (Impossible e) = show e

instance Exception ImpossibleError where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

toImpossibleError :: (Exception e, Pretty e) => e -> SomeException
toImpossibleError = toException . Impossible

fromImpossibleError :: (Exception e, Pretty e) => SomeException -> Maybe e
fromImpossibleError exc = do
    Impossible e <- fromException exc
    cast e

data OutputDiffUnparseable = OutputDiffUnparseable String
    deriving (Show, Typeable)

instance Pretty OutputDiffUnparseable where
    pretty (OutputDiffUnparseable txt) = Pretty.reflow $
      "Failed to parse numdiff.awk output: " <> T.pack txt

instance Exception OutputDiffUnparseable where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data QueryPlanUnparseable = QueryPlanUnparseable
    deriving (Show, Typeable)

instance Pretty QueryPlanUnparseable where
    pretty QueryPlanUnparseable = Pretty.reflow
      "Failed to parse/render SQLite query plan."

instance Exception QueryPlanUnparseable where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data QueryResultUnparseable = QueryResultUnparseable [PersistValue] [SqlType]
    deriving (Show, Typeable)

instance Pretty QueryResultUnparseable where
    pretty (QueryResultUnparseable got expected) = Pretty.vsep
        [ Pretty.reflow
            "Query returned an unexpected number/type of arguments!"
        , ""
        , "Expected:" <+> renderPrettyList expectedPretty
        , ""
        , "Got:" <+> renderPrettyList gotPretty
        ]
      where
        prettyType :: SqlType -> Doc a
        prettyType = pretty . drop 3 . show

        expectedPretty :: [Doc a]
        expectedPretty = map prettyType expected

        gotPretty :: [Doc a]
        gotPretty = map persistValToPrettyType got

        persistValToPrettyType :: PersistValue -> Doc a
        persistValToPrettyType val = case val of
            PersistText{} -> prettyType SqlString
            PersistByteString{} -> prettyType SqlBlob
            PersistInt64{} -> prettyType SqlInt64
            PersistDouble{} -> prettyType SqlReal
            PersistRational{} -> "Rational"
            PersistBool{} -> prettyType SqlBool
            PersistDay{} -> prettyType SqlDay
            PersistTimeOfDay{} -> prettyType SqlTime
            PersistUTCTime{} -> prettyType SqlDayTime
            PersistNull -> "Null"
            _ -> "Unknown Type"

instance Exception QueryResultUnparseable where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data QueryReturnedZeroResults = QueryReturnedZeroResults
    deriving (Show, Typeable)

instance Pretty QueryReturnedZeroResults where
    pretty QueryReturnedZeroResults = Pretty.reflow
        "A query that should always have results returned zero results."

instance Exception QueryReturnedZeroResults where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data QueryResultMismatch where
    QueryResultMismatch
        :: (Pretty e, Show e) => [e] -> [e] -> QueryResultMismatch
    deriving (Typeable)

deriving instance Show QueryResultMismatch

instance Pretty QueryResultMismatch where
    pretty (QueryResultMismatch expected got) = Pretty.vsep
        [ "Query returned mismatched output."
        , "Expected:" <+> renderList expected
        , "Got:" <+> renderList got
        ]

instance Exception QueryResultMismatch where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data ModelInfoParseFailed = ModelInfoParseFailed Text
    deriving (Show, Typeable)

instance Pretty ModelInfoParseFailed where
    pretty (ModelInfoParseFailed parseError) = Pretty.vsep
        [ "An error occured while parsing model info from the training script."
        , "Parse error:"
        , pretty parseError
        ]

instance Exception ModelInfoParseFailed where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

newtype PrettyUnicodeException = PrettyUnicodeException UnicodeException
    deriving (Show, Typeable)

instance Pretty PrettyUnicodeException where
    pretty (PrettyUnicodeException exc) = Pretty.vsep
        [ Pretty.reflow "Encountered a unicode decoding error while handling \
                        \SQLite error message."
        , "Error:"
        , pretty . displayException $ exc
        ]

instance Exception PrettyUnicodeException where
    toException = toImpossibleError
    fromException = fromImpossibleError
    displayException = show . pretty

data SchemaException where
    SchemaException :: (Exception e, Pretty e) => e -> SchemaException
    deriving (Typeable)

instance Pretty SchemaException where
    pretty (SchemaException e) = pretty e

instance Show SchemaException where
    show (SchemaException e) = show e

instance Exception SchemaException where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

toSchemaException :: (Exception e, Pretty e) => e -> SomeException
toSchemaException = toException . SchemaException

fromSchemaException :: (Exception e, Pretty e) => SomeException -> Maybe e
fromSchemaException exc = do
    SchemaException e <- fromException exc
    cast e

data SchemaWrong = WrongSchema
    deriving (Show, Typeable)

instance Pretty SchemaWrong where
    pretty WrongSchema = Pretty.vsep
        [ Pretty.reflow "Found wrong database schema for schema version!"
        , Pretty.line
        , Pretty.reflow "Are you sure the input database is an SQLite \
                        \database created by this program?"
        ]

instance Exception SchemaWrong where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty

data AbortMigration = AbortMigration
    deriving (Show, Typeable)

instance Pretty AbortMigration where
    pretty AbortMigration = "Aborted: Migration failed!"

instance Exception AbortMigration where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty

data ForeignKeyViolation = ForeignKeyViolation
    deriving (Show, Typeable)

instance Pretty ForeignKeyViolation where
    pretty ForeignKeyViolation = Pretty.vsep
        [ Pretty.reflow "Found violated foreign key constraints. Data may \
                        \have been corrupted by manual database modification!"
        , Pretty.line
        , Pretty.reflow "Repair foreign key constraints before continuing."
        ]

instance Exception ForeignKeyViolation where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty

data UniquenessViolation = UniqueViolation Text Text
    deriving (Show, Typeable)

instance Pretty UniquenessViolation where
    pretty (UniqueViolation expected found) = Pretty.vsep
        [ Pretty.reflow "Found unexpected conflicting value on unique insert!"
        , Pretty.line, "Expected:", Pretty.line
        , Pretty.reflow expected, Pretty.line, Pretty.line, "Found:"
        , Pretty.line, Pretty.reflow found
        ]

instance Exception UniquenessViolation where
    toException = toSchemaException
    fromException = fromSchemaException
    displayException = show . pretty

data SqlException where
    SqlException :: (Exception e, Pretty e) => e -> SqlException
    deriving (Typeable)

instance Pretty SqlException where
    pretty (SqlException e) = pretty e

instance Show SqlException where
    show (SqlException e) = show e

instance Exception SqlException where
    toException = toBenchmarkException
    fromException = fromBenchmarkException
    displayException = show . pretty

toSqlException :: (Exception e, Pretty e) => e -> SomeException
toSqlException = toException . SqlException

fromSqlException :: (Exception e, Pretty e) => SomeException -> Maybe e
fromSqlException exc = do
    SqlException e <- fromException exc
    cast e

data AbortTransaction = AbortTransaction Text
    deriving (Show, Typeable)

instance Pretty AbortTransaction where
    pretty (AbortTransaction txt) = Pretty.vsep
        [ "Aborted transaction:", Pretty.reflow txt ]

instance Exception AbortTransaction where
    toException = toSqlException
    fromException = fromSqlException
    displayException = show . pretty

data ExpectedSingleValue = ExpectedSingleValue Text
    deriving (Show, Typeable)

instance Pretty ExpectedSingleValue where
    pretty (ExpectedSingleValue q) = Pretty.vsep
        [ "Query:", pretty q, ""
        , Pretty.reflow
            "Query should always return a single value, but got multiple!"
        , Pretty.reflow "This should be impossible, please report a bug."
        ]

instance Exception ExpectedSingleValue where
    toException = toSqlException
    fromException = fromSqlException
    displayException = show . pretty

newtype PrettySqliteException = PrettySqliteException SqliteException
    deriving (Show, Typeable)

instance Pretty PrettySqliteException where
    pretty (PrettySqliteException SqliteException{..}) = Pretty.vsep
        [ Pretty.viaShow seError <> pretty seDetails
        , Pretty.vsep . map pretty . T.splitOn "\\n" $
            T.replace "\\\"" "\"" seFunctionName
        ]

instance Exception PrettySqliteException where
    toException = toSqlException
    fromException = fromSqlException
    displayException = show . pretty
