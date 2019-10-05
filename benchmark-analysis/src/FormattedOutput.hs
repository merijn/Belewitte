{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module FormattedOutput (renderColumns, renderOutput, outputSink) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit (ConduitT, Void, (.|), runConduit, yield)
import qualified Data.Conduit.Combinators as C
import Database.Persist.Class (persistFieldDef)
import Database.Persist.Types (fieldHaskell, unHaskellName)
import Lens.Micro.Extras (view)
import System.IO (stdout)

import Core
import Pretty.Columns
import Query (MonadQuery)
import Sql

columnName :: PersistEntity r => EntityField r a -> Text
columnName = unHaskellName . fieldHaskell . persistFieldDef

queryColumnInfo
    :: (PrettyColumns a, MonadQuery m)
    => ColumnInfo a -> m (ColumnInfo a, (Avg, Max))
queryColumnInfo ColSeparator = return (ColSeparator, (Avg 1, Max 3))
queryColumnInfo col@(ColInfo field _) =
    annotateColumn <$> Sql.getFieldLength field
  where
    columnSize = Max . T.length . columnName $ field
    annotateColumn (avgVal, maxVal) = (col, (avgVal, max columnSize maxVal))

columnFormatter
    :: (MonadQuery m, PrettyColumns a) => m (Text, Entity a -> Text)
columnFormatter = do
    annotatedColumns <- traverse queryColumnInfo prettyColumnInfo
    let renderEntity = foldMap padColumn annotatedColumns
        headerText = foldMap header annotatedColumns
    return (headerText, renderEntity)
  where
    padText :: Int -> Text -> Text
    padText n input = input <> T.replicate (n - T.length input) " "

    header :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Text
    header (ColSeparator, (_, Max n)) = T.replicate n " "
    header (ColInfo field _, (_, Max n)) = padText n $ columnName field

    padColumn
        :: PrettyColumns a => (ColumnInfo a, (Avg, Max)) -> Entity a -> Text
    padColumn (ColSeparator, (_, Max n)) _ = T.replicate n " "
    padColumn (ColInfo f toText, (_, Max n)) val = padText n col
      where
        col = toText . view (Sql.fieldLens f) $ val

renderColumns :: PrettyColumns a => [Filter a] -> [SelectOpt a] -> SqlM ()
renderColumns filts order = do
    (header, f) <- columnFormatter
    let columnSource = do
            yield header
            Sql.selectSource filts order .| C.map f
    runConduit $ columnSource .| C.unlines .| outputSink

renderOutput :: MonadIO m => ConduitT () Text m () -> m ()
renderOutput producer = runConduit $ producer .| outputSink

outputSink :: MonadIO m => ConduitT Text Void m ()
outputSink = C.encodeUtf8 .| C.sinkHandle stdout
