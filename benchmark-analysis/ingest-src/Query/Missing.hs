{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Query.Missing
    ( MissingRun(..)
    , ExtraVariantInfo(..)
    , ValidationVariant(..)
    , missingBenchmarkQuery
    , validationVariantQuery
    , validationRunQuery
    ) where

import Data.Maybe (fromMaybe)
import Data.String.Interpolate.IsString (i)

import Core
import ProcessPool (Job, makeTimingJob)
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
    } deriving (Functor, Show)

data ExtraVariantInfo = ExtraVariantInfo
    { extraVariantHash :: !(Maybe Hash)
    , extraVariantSteps :: {-# UNPACK #-} !Int
    } deriving (Show)

data ValidationVariant = ValidationVariant
    { validationAlgorithmId :: {-# UNPACK #-} !(Key Algorithm)
    , validationVariantId :: {-# UNPACK #-} !(Key Variant)
    , validationCommit :: {-# UNPACK #-} !CommitId
    , validationMissingCount :: {-# UNPACK #-} !Int64
    , validationArgs :: ![Text]
    } deriving (Show)

validationVariantQuery :: Key Platform -> Query (Job ValidationVariant)
validationVariantQuery platformId = Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "validationVariantQuery"

    converter :: MonadConvert m => [PersistValue] -> m (Job ValidationVariant)
    converter [ PersistInt64 (toSqlKey -> validationAlgorithmId)
              , PersistText algoName
              , PersistInt64 (toSqlKey -> validationVariantId)
              , (fromPersistValue -> Right validationCommit)
              , PersistInt64 validationMissingCount
              , (fromPersistValue -> Right variantFlags)
              , PersistText graphPath
              ] = return $ makeTimingJob ValidationVariant{..}
                            validationVariantId
                            Nothing
                            ("-k switch" : validationArgs)
      where
        validationArgs =
          [ "-a " <> algoName
          , fromMaybe "" variantFlags
          , "-n", "1", "--validate"
          , graphPath
          ]

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlString, SqlInt64, SqlString
        , SqlInt64, SqlString, SqlString ]

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue platformId ]

    queryText = [i|
SELECT Algorithm.id
     , Algorithm.name
     , Variant.id
     , Run.algorithmVersion
     , Run.missingCount
     , VariantConfig.flags
     , Graph.path
FROM (SELECT variantId, RunConfig.algorithmVersion, COUNT(*) AS missingCount
      FROM Run
      INNER JOIN RunConfig
      ON Run.runConfigId = RunConfig.id
      WHERE NOT validated AND RunConfig.platformId = ?
      GROUP BY variantId, RunConfig.algorithmVersion) AS Run

INNER JOIN Variant
ON Run.variantId = Variant.id

INNER JOIN Algorithm
ON Variant.algorithmId = Algorithm.id

INNER JOIN VariantConfig
ON Variant.variantConfigId = VariantConfig.id

INNER JOIN Graph
ON Variant.graphId = Graph.id
|]

validationRunQuery
    :: ValidationVariant -> Key Platform -> Query (MissingRun (Key Run))
validationRunQuery ValidationVariant{..} platformId =
    Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "validationRunQuery"

    converter :: MonadConvert m => [PersistValue] -> m (MissingRun (Key Run))
    converter [ PersistInt64 (toSqlKey -> missingRunImplId)
              , PersistText missingRunImplName
              , (fromPersistValue -> Right implFlags)
              , PersistInt64 (toSqlKey -> missingRunExtraInfo)
              ] = return MissingRun{..}
      where
        implArgs = fromMaybe ("-k " <> missingRunImplName) implFlags
        missingRunAlgorithmId = validationAlgorithmId
        missingRunVariantId = validationVariantId
        missingRunArgs = implArgs : "--validate" : validationArgs

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlString, SqlString, SqlInt64 ]

    commonTableExpressions :: [CTE]
    commonTableExpressions = []

    params :: [PersistValue]
    params = [ toPersistValue platformId
             , toPersistValue validationAlgorithmId
             , toPersistValue validationVariantId
             ]

    queryText = [i|
SELECT Implementation.id, Implementation.name, Implementation.flags, Run.id
FROM Run

INNER JOIN Implementation
ON Run.implId = Implementation.id

INNER JOIN RunConfig
ON Run.runConfigId = RunConfig.id

WHERE RunConfig.platformId = ?
  AND Run.algorithmId = ?
  AND Run.variantId = ?
  AND NOT Run.validated
|]

missingBenchmarkQuery :: Key RunConfig -> Query (MissingRun ExtraVariantInfo)
missingBenchmarkQuery runConfigId = Query{convert = Simple converter, ..}
  where
    queryName :: Text
    queryName = "missingBenchmarkQuery"

    converter
        :: MonadConvert m => [PersistValue] -> m (MissingRun ExtraVariantInfo)
    converter [ PersistInt64 numRepeats
              , PersistText graphPath
              , PersistInt64 (toSqlKey -> missingRunAlgorithmId)
              , PersistText algoName
              , PersistInt64 (toSqlKey -> missingRunVariantId)
              , (fromPersistValue -> Right variantFlags)
              , (fromPersistValue -> Right extraVariantHash)
              , (fromPersistValue -> Right extraVariantSteps)
              , PersistInt64 (toSqlKey -> missingRunImplId)
              , PersistText missingRunImplName
              , (fromPersistValue -> Right implFlags)
              ] = return MissingRun{..}
      where
        missingRunExtraInfo = ExtraVariantInfo{..}
        missingRunArgs =
          [ "-a " <> algoName
          , fromMaybe ("-k " <> missingRunImplName) implFlags
          , fromMaybe "" variantFlags
          , "-n " <> showText numRepeats
          , graphPath
          ]

    converter actualValues = logThrowM $ QueryResultUnparseable actualValues
        [ SqlInt64, SqlString, SqlInt64, SqlString, SqlInt64, SqlString
        , SqlBlob, SqlInt64, SqlInt64, SqlString, SqlString ]

    commonTableExpressions :: [CTE]
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
              , Variant.maxStepId
              , Implementation.id
              , Implementation.name
              , Implementation.flags
FROM RunConfig

INNER JOIN Algorithm
ON RunConfig.algorithmId = Algorithm.id

INNER JOIN Graph
ON RunConfig.datasetId = Graph.datasetId

INNER JOIN Variant
ON RunConfig.algorithmId = Variant.algorithmId
AND Graph.id = Variant.graphId

INNER JOIN VariantConfig
ON RunConfig.algorithmId = VariantConfig.algorithmId
AND Variant.variantConfigId = VariantConfig.id

INNER JOIN Implementation
ON RunConfig.algorithmId = Implementation.algorithmId

LEFT JOIN Run
ON Variant.id = Run.variantId
AND Implementation.id = Run.implId
AND RunConfig.id = Run.runConfigId

WHERE RunConfig.id = ? AND Variant.retryCount < 5 AND Run.runConfigId IS NULL
|]
