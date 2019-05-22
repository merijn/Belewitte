{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Exceptions
    ( Exception(..)
    , Pretty(pretty)
    , SqlType(..)
    , module Exceptions
    ) where

import Control.Monad.Catch
    (Exception(..), MonadCatch, MonadThrow, SomeException)
import Control.Monad.Logger (MonadLogger, logErrorN)
import qualified Control.Monad.Catch as Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Error (UnicodeException(..))
import Data.Text.Prettyprint.Doc (Doc, Pretty(pretty), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Text.Prettyprint.Doc.Util as Pretty
import Data.Typeable (Typeable, cast)
import Database.Persist.Types (SqlType(..), PersistValue(..))
import Database.Sqlite (SqliteException(..))

mapException
    :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f = Except.handle (Except.throwM . f)

logThrowM :: (Exception e, MonadLogger m, MonadThrow m, Pretty e) => e -> m r
logThrowM exc = do
    logErrorN . Pretty.renderStrict . layoutException $ exc
    Except.throwM exc
  where
    layoutException :: Pretty e => e -> Pretty.SimpleDocStream a
    layoutException = Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

displayLogThrowM :: (Exception e, MonadLogger m, MonadThrow m) => e -> m r
displayLogThrowM e = do
    logErrorN . T.pack . displayException $ e
    Except.throwM e

renderList :: Pretty e => [e] -> Doc a
renderList = renderPrettyList . map pretty

renderPrettyList :: [Doc a] -> Doc a
renderPrettyList = Pretty.align . Pretty.encloseSep "[" "]" ", "

data BenchmarkException where
    BenchmarkException :: (Exception e, Pretty e) => e -> BenchmarkException
    deriving (Typeable)

instance Show BenchmarkException where
    show (BenchmarkException e) = show e

instance Exception BenchmarkException where
    displayException (BenchmarkException e) = show $ pretty e

toBenchmarkException :: (Exception e, Pretty e) => e -> SomeException
toBenchmarkException = toException . BenchmarkException

fromBenchmarkException :: (Exception e, Pretty e) => SomeException -> Maybe e
fromBenchmarkException exc = do
    BenchmarkException e <- fromException exc
    cast e

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

data PatternFailed = PatternFailed (Either Text Text)
    deriving (Show, Typeable)

instance Pretty PatternFailed where
    pretty (PatternFailed (Left txt)) = mconcat
        [ Pretty.group $ mconcat
            [ Pretty.reflow "Unexpected pattern match failure:"
            , Pretty.line
            , Pretty.reflow txt
            ]
        , Pretty.hardline
        , Pretty.reflow "Report this missing diagnostic as a bug."
        ]

    pretty (PatternFailed (Right txt)) = Pretty.group $ mconcat
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

data StdinDisappeared = StdinDisappeared
    deriving (Show, Typeable)

instance Pretty StdinDisappeared where
    pretty StdinDisappeared = Pretty.reflow
        "stdin was unexpectedly closed while waiting for user input!"

instance Exception StdinDisappeared where
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

data ExpectedSingleValue = ExpectedSingleValue Text String
    deriving (Show, Typeable)

instance Pretty ExpectedSingleValue where
    pretty (ExpectedSingleValue q v) = Pretty.vsep
        [ "Query:", pretty q, ""
        , Pretty.reflow "Query should always return a single value!"
        , ""
        , "Got:" <+> renderList v
        , ""
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
        [ pretty seFunctionName <> ":" <+> Pretty.viaShow seError
        , pretty seDetails
        ]

instance Exception PrettySqliteException where
    toException = toSqlException
    fromException = fromSqlException
    displayException = show . pretty
