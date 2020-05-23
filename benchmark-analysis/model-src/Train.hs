{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Train
    ( LegacyConfig(..)
    , ModelDescription(..)
    , StepInfoConfig(..)
    , TrainingConfig(..)
    , getTotalQuery
    , getTrainingQuery
    , getValidationQuery
    , getModelTrainingConfig
    , setTrainingConfigDatasets
    , setTrainingConfigPlatform
    , setTrainingConfigSkipIncomplete
    , trainModel
    ) where

import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Trans.Resource (register, release)
import Data.Binary.Get (getDoublehost, getRemainingLazyByteString, runGet)
import Data.Binary.Put (putDoublehost, putInt64host, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (hClose)
import Text.Megaparsec (Parsec, parse, between, sepBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

import Core
import Model (byteStringToModel)
import Model.Stats (UnknownSet(..), ModelStats(..))
import Utils.Process
    (ReadWrite(..), runProcessCreation_, withPipe, withProcess)
import Query
import RuntimeData (getModelScript)
import Schema
import StepQuery
    (QueryMode(..), StepInfo(..), StepInfoConfig(..), stepInfoQuery)
import Sql (MonadSql, (==.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

data TrainingConfig
    = TrainConfig StepInfoConfig
    | LegacyTrainConfig LegacyConfig
    deriving (Show)

data LegacyConfig = LegacyConfig
    { legacyAlgorithm :: Key Algorithm
    , legacyPlatform :: Key Platform
    , legacyCommit :: CommitId
    , legacyGraphProps :: Set Text
    , legacyStepProps :: Set Text
    , legacyFraction :: Double
    , legacySeed :: Int64
    , legacyDatasets :: Maybe (Set (Key Dataset))
    , legacyTimestamp :: UTCTime
    } deriving (Show)

setTrainingConfigDatasets
    :: Maybe (Set (Key Dataset)) -> TrainingConfig -> TrainingConfig
setTrainingConfigDatasets datasets trainConfig = case trainConfig of
    TrainConfig cfg@StepInfoConfig{stepInfoDatasets} -> TrainConfig cfg
      { stepInfoDatasets = updateDatasets stepInfoDatasets }
    LegacyTrainConfig cfg@LegacyConfig{legacyDatasets} -> LegacyTrainConfig cfg
      { legacyDatasets = updateDatasets legacyDatasets }
  where
    updateDatasets
        | maybe True S.null datasets = id
        | otherwise = const datasets

setTrainingConfigPlatform :: Key Platform -> TrainingConfig -> TrainingConfig
setTrainingConfigPlatform platformId trainConfig = case trainConfig of
    TrainConfig cfg@StepInfoConfig{} -> TrainConfig cfg
      { stepInfoPlatform = platformId }
    LegacyTrainConfig cfg@LegacyConfig{} -> LegacyTrainConfig cfg
      { legacyPlatform = platformId }

setTrainingConfigSkipIncomplete :: Bool -> TrainingConfig -> TrainingConfig
setTrainingConfigSkipIncomplete val trainConfig = case trainConfig of
    TrainConfig cfg@StepInfoConfig{} -> TrainConfig cfg
      { stepInfoFilterIncomplete = val }
    LegacyTrainConfig _ -> trainConfig

data ModelDescription = ModelDesc
    { modelName :: Text
    , modelPrettyName :: Maybe Text
    , modelDescription :: Maybe Text
    , modelTrainConfig :: StepInfoConfig
    }

splitQuery
    :: MonadQuery m => TrainingConfig -> m (Query StepInfo, Query StepInfo)
splitQuery (TrainConfig cfg) =
    return (stepInfoQuery trainConfig, stepInfoQuery validateConfig)
  where
    trainConfig = cfg { stepInfoQueryMode = Train }
    validateConfig = cfg { stepInfoQueryMode = Validate }

splitQuery cfg@(LegacyTrainConfig LegacyConfig{..}) = do
    rowCount <- runSqlQueryCount query

    let trainingSize :: Int
        trainingSize = round (fromIntegral rowCount * legacyFraction)

    return $ randomizeQuery legacySeed trainingSize query
  where
    query = getTotalQuery cfg

getTrainingQuery :: MonadQuery m => TrainingConfig -> m (Query StepInfo)
getTrainingQuery = fmap fst . splitQuery

getValidationQuery :: MonadQuery m => TrainingConfig -> m (Query StepInfo)
getValidationQuery = fmap snd . splitQuery

getTotalQuery :: TrainingConfig -> Query StepInfo
getTotalQuery (TrainConfig cfg) = stepInfoQuery cfg{ stepInfoQueryMode = All }

getTotalQuery (LegacyTrainConfig LegacyConfig{..}) =
    stepInfoQuery StepInfoConfig
        { stepInfoAlgorithm = legacyAlgorithm
        , stepInfoPlatform = legacyPlatform
        , stepInfoQueryMode = All
        , stepInfoCommit = legacyCommit
        , stepInfoGraphProps = legacyGraphProps
        , stepInfoStepProps = legacyStepProps
        , stepInfoSeed = legacySeed
        , stepInfoTimestamp = legacyTimestamp
        , stepInfoDatasets = legacyDatasets
        , stepInfoFilterIncomplete = False
        , ..
        }
  where
    stepInfoGraphs, stepInfoVariants, stepInfoSteps :: Percentage
    stepInfoGraphs = $$(validRational 1)
    stepInfoVariants = $$(validRational 1)
    stepInfoSteps = $$(validRational 1)

getModelTrainingConfig :: MonadSql m => Key PredictionModel -> m TrainingConfig
getModelTrainingConfig modelId = SqlTrans.runTransaction $ do
    PredictionModel{..} <- SqlTrans.getJust modelId

    modelProps <- SqlTrans.selectSource [ModelPropertyModelId ==. modelId] [] $
        C.map (modelPropertyPropId . SqlTrans.entityVal)
        .| C.foldMap S.singleton

    let checkKey :: Entity PropertyName -> Bool
        checkKey (Entity k _) = S.member k modelProps

    graphProps <-
        SqlTrans.selectSource [PropertyNameIsStepProp ==. False] [] $
            C.filter checkKey
            .| C.foldMap (S.singleton . propertyNameProperty . entityVal)

    stepProps <-
        SqlTrans.selectSource [PropertyNameIsStepProp ==. True] [] $
            C.filter checkKey
            .| C.foldMap (S.singleton . propertyNameProperty . entityVal)

    trainingDatasets <-
        SqlTrans.selectSource [ModelTrainDatasetModelId ==. modelId] [] $
            C.map (modelTrainDatasetDatasetId . SqlTrans.entityVal)
            .| C.foldMap S.singleton

    if predictionModelLegacyTrainFraction /= 0.0
       then return $ LegacyTrainConfig LegacyConfig
            { legacyAlgorithm = predictionModelAlgorithmId
            , legacyPlatform = predictionModelPlatformId
            , legacyCommit = predictionModelAlgorithmVersion
            , legacyGraphProps = graphProps
            , legacyStepProps = stepProps
            , legacyFraction = predictionModelLegacyTrainFraction
            , legacySeed = predictionModelTrainSeed
            , legacyDatasets = Just trainingDatasets
            , legacyTimestamp = predictionModelTimestamp
            }
       else return $ TrainConfig StepInfoConfig
            { stepInfoQueryMode = All
            , stepInfoAlgorithm = predictionModelAlgorithmId
            , stepInfoPlatform = predictionModelPlatformId
            , stepInfoCommit = predictionModelAlgorithmVersion
            , stepInfoGraphProps = graphProps
            , stepInfoStepProps = stepProps
            , stepInfoSeed = predictionModelTrainSeed
            , stepInfoDatasets = Just trainingDatasets
            , stepInfoFilterIncomplete = predictionModelSkipIncomplete
            , stepInfoGraphs = predictionModelTrainGraphs
            , stepInfoVariants = predictionModelTrainVariants
            , stepInfoSteps = predictionModelTrainSteps
            , stepInfoTimestamp = predictionModelTimestamp
            }

trainModel
    :: (MonadMask m, MonadQuery m)
    => ModelDescription -> m (Key PredictionModel, Model)
trainModel ModelDesc{..} = do
    numEntries <- runSqlQueryCount trainQuery

    propCount <- runSqlQueryConduit trainQuery C.head >>= \case
        Just (v, _) -> return $ VS.length v
        Nothing -> logThrowM . PatternFailed $
            "Unable to compute property count for model"

    timestamp <- liftIO getCurrentTime
    (model, ModelStats{..}) <- runProcessCreation_ $ do
        (resultsFd, resultsHnd) <- withPipe Write
        (unknownsFd, unknownsHnd) <- withPipe Read

        modelProcess <- lift $ getModelScript
            [ "--entries"
            , show numEntries
            , "--properties"
            , show propCount
            , "--results-fd"
            , resultsFd
            , "--unknowns-fd"
            , unknownsFd
            ]

        withProcess modelProcess $ \(propSink, closeSink) modelHnd -> do
            processIn <- register closeSink

            let propertySink = ZipSink $ do
                    C.map (putProps . fst) .| propSink
                    release processIn

                resultSink = ZipSink $ do
                    C.map (putResults . snd) .| C.sinkHandle resultsHnd
                    liftIO $ hClose resultsHnd

                combinedSink = getZipSink (propertySink *> resultSink)

            runSqlQueryConduit trainQuery combinedSink

            (featureImportance, model) <- liftIO $
                getResult propCount <$> BS.hGetContents modelHnd

            unknownTxt <- liftIO $ T.hGetContents unknownsHnd
            (modelUnknownCount, modelUnknownPreds) <- getUnknowns unknownTxt

            let (graphProps, stepProps) =
                    VS.splitAt (S.size stepInfoGraphProps) featureImportance

                labelledGraphProps =
                    zip (S.toAscList stepInfoGraphProps) (VS.toList graphProps)

                labelledStepProps =
                    zip (S.toAscList stepInfoStepProps) (VS.toList stepProps)

            modelGraphPropImportance <- fmap M.fromList $
                forM labelledGraphProps $ \(propName, value) -> do
                    propId <- Sql.getJustKeyBy $ UniqProperty propName
                    return (propId, value)

            modelStepPropImportance <- fmap M.fromList $
                forM labelledStepProps $ \(propName, value) -> do
                    propId <- Sql.getJustKeyBy $ UniqProperty propName
                    return (propId, value)

            return (model, ModelStats{..})

    modelId <- SqlTrans.runTransaction $ do
        modelId <- SqlTrans.insert $ PredictionModel
            { predictionModelPlatformId = stepInfoPlatform
            , predictionModelAlgorithmId = stepInfoAlgorithm
            , predictionModelAlgorithmVersion = stepInfoCommit
            , predictionModelName = modelName
            , predictionModelPrettyName = modelPrettyName
            , predictionModelDescription = modelDescription
            , predictionModelModel = model
            , predictionModelSkipIncomplete = stepInfoFilterIncomplete
            , predictionModelLegacyTrainFraction = 0
            , predictionModelTrainGraphs = stepInfoGraphs
            , predictionModelTrainVariants = stepInfoVariants
            , predictionModelTrainSteps = stepInfoSteps
            , predictionModelTrainSeed = stepInfoSeed
            , predictionModelTotalUnknownCount = modelUnknownCount
            , predictionModelTimestamp = timestamp
            }

        forM_ (fromMaybe mempty stepInfoDatasets) $
            SqlTrans.insert_ . ModelTrainDataset modelId

        forM_ (M.toList modelGraphPropImportance) $
            SqlTrans.insert_ . uncurry (ModelProperty modelId)

        forM_ (M.toList modelStepPropImportance) $
            SqlTrans.insert_ . uncurry (ModelProperty modelId)

        forM_ (M.toList modelUnknownPreds) $ \(setId, UnknownSet{..}) -> do
            unknownId <- SqlTrans.insert $
                UnknownPrediction modelId stepInfoAlgorithm setId unknownSetOccurence

            forM_ (S.toList unknownSetImpls) $ \i -> do
                implKey <- SqlTrans.validateKey "Implementation" (fromSqlKey i)
                SqlTrans.insert_ $
                    UnknownPredictionSet unknownId implKey stepInfoAlgorithm

        return modelId

    return (modelId, model)
  where
    StepInfoConfig{..} = modelTrainConfig

    trainQuery = reduceInfo <$> stepInfoQuery modelTrainConfig

    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

putProps :: Vector Double -> ByteString
putProps = LBS.toStrict . runPut . VS.mapM_ putDoublehost

putResults :: Int64 -> ByteString
putResults = LBS.toStrict . runPut . putInt64host

getResult :: Int -> ByteString -> (Vector Double, Model)
getResult columnCount = runGet parseBlock . LBS.fromStrict
  where
    parseBlock = do
        dbls <- VS.replicateM columnCount getDoublehost
        bs <- LBS.toStrict <$> getRemainingLazyByteString
        return (dbls, byteStringToModel bs)

getUnknowns
    :: (MonadLogger m, MonadThrow m)
    => Text -> m (Int, Map Int64 UnknownSet)
getUnknowns txt = case parse parseUnknowns "getUnknowns" txt of
    Left e -> logThrowM . ModelInfoParseFailed . T.pack $ errorBundlePretty e
    Right r -> return r

parseUnknowns :: Parsec Void Text (Int, Map Int64 UnknownSet)
parseUnknowns = do
    unknownCount <- decimal <* eol
    uniqUnknownSets <- decimal <* eol
    unknowns <- M.unions <$> replicateM uniqUnknownSets parseUnknownSet
    return (unknownCount, unknowns)

parseUnknownSet :: Parsec Void Text (Map Int64 UnknownSet)
parseUnknownSet = do
    setId <- decimal <* string " : "
    impls <- between (char '(') (char ')') $
        S.fromList <$> sepBy1 implParser (string ", ")
    string " : "
    count <- decimal
    eol
    return $ M.singleton setId (UnknownSet count impls)
  where
    implParser = toSqlKey <$> decimal
