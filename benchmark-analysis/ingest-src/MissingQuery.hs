{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module MissingQuery (MissingRun(..), missingQuery) where

import Data.Maybe (fromMaybe)
import Data.String.Interpolate.IsString (i)

import Core
import Query
import Schema
import Sql (fromPersistValue)

data MissingRun = MissingRun
    { missingRunAlgorithmId :: {-# UNPACK #-} !(Key Algorithm)
    , missingRunImplId :: {-# UNPACK #-} !(Key Implementation)
    , missingRunImplName :: {-# UNPACK #-} !Text
    , missingRunVariantId :: {-# UNPACK #-} !(Key Variant)
    , missingRunVariantResult :: !(Maybe Hash)
    , missingRunArgs :: ![Text]
    } deriving (Show)

missingQuery :: Key RunConfig -> Query MissingRun
missingQuery runConfigId = Query{..}
  where
    queryName :: Text
    queryName = "missingQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m MissingRun
    convert [ PersistInt64 numRepeats
            , PersistText graphPath
            , PersistInt64 (toSqlKey -> missingRunAlgorithmId)
            , PersistText algoName
            , PersistInt64 (toSqlKey -> missingRunVariantId)
            , (fromPersistValue -> Right variantFlags)
            , (fromPersistValue -> Right missingRunVariantResult)
            , PersistInt64 (toSqlKey -> missingRunImplId)
            , PersistText missingRunImplName
            , (fromPersistValue -> Right implFlags)
            ] = return $ MissingRun{..}
      where
        missingRunArgs =
          [ "-a " <> algoName
          , fromMaybe ("-k " <> missingRunImplName) implFlags
          , fromMaybe "" variantFlags
          , "-n " <> showText numRepeats
          , graphPath
          ]

    convert actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlString, SqlInt64, SqlString, SqlInt64, SqlString
        , SqlBlob, SqlInt64, SqlString, SqlString ]

    cteParams :: [PersistValue]
    cteParams = []

    commonTableExpressions :: [Text]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue runConfigId ]

    queryText = [i|
SELECT DISTINCT RunConfig.repeats
              , Graph.path
              , Algorithm.id
              , Algorithm.name
              , Variant.id
              , Variant.flags
              , Variant.result
              , Implementation.id
              , Implementation.name
              , Implementation.flags
FROM RunConfig

INNER JOIN Algorithm
ON RunConfig.algorithmId = Algorithm.id

INNER JOIN Dataset
ON RunConfig.datasetId = Dataset.id

INNER JOIN Graph
ON Dataset.id = Graph.datasetId

INNER JOIN Variant
ON Graph.id = Variant.graphId

INNER JOIN Implementation
ON RunConfig.algorithmId = Implementation.algorithmId

LEFT JOIN Run
ON Variant.id = Run.variantId
AND Implementation.id = Run.implId
AND RunConfig.id = Run.runConfigId

WHERE RunConfig.id = ? AND Run.runConfigId IS NULL
|]
