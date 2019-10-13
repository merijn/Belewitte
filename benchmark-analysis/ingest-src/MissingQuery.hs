{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module MissingQuery (MissingRun(..), missingBenchmarkQuery) where

import Data.Maybe (fromMaybe)
import Data.String.Interpolate.IsString (i)

import Core
import Query
import Schema
import Sql (fromPersistValue)

data MissingRun a = MissingRun
    { missingRunAlgorithmId :: {-# UNPACK #-} !(Key Algorithm)
    , missingRunImplId :: {-# UNPACK #-} !(Key Implementation)
    , missingRunImplName :: {-# UNPACK #-} !Text
    , missingRunVariantId :: {-# UNPACK #-} !(Key Variant)
    , missingRunArgs :: ![Text]
    , missingRunExtraInfo :: !a
    } deriving (Show)

missingBenchmarkQuery :: Key RunConfig -> Query (MissingRun (Maybe Hash))
missingBenchmarkQuery runConfigId = Query{..}
  where
    queryName :: Text
    queryName = "missingQuery"

    convert
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => [PersistValue] -> m (MissingRun (Maybe Hash))
    convert [ PersistInt64 numRepeats
            , PersistText graphPath
            , PersistInt64 (toSqlKey -> missingRunAlgorithmId)
            , PersistText algoName
            , PersistInt64 (toSqlKey -> missingRunVariantId)
            , (fromPersistValue -> Right variantFlags)
            , (fromPersistValue -> Right missingRunExtraInfo)
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
              , VariantConfig.flags
              , Variant.result
              , Implementation.id
              , Implementation.name
              , Implementation.flags
FROM RunConfig

INNER JOIN Algorithm
ON RunConfig.algorithmId = Algorithm.id

INNER JOIN Graph
ON RunConfig.datasetId = Graph.datasetId

INNER JOIN Variant
ON Graph.id = Variant.graphId

INNER JOIN VariantConfig
ON Variant.variantConfigId = VariantConfig.id

INNER JOIN Implementation
ON RunConfig.algorithmId = Implementation.algorithmId

LEFT JOIN Run
ON Variant.id = Run.variantId
AND Implementation.id = Run.implId
AND RunConfig.id = Run.runConfigId

WHERE RunConfig.id = ? AND Variant.retryCount < 5 AND Run.runConfigId IS NULL
|]
