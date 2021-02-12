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
    , TrainStepConfig(..)
    , getTotalQuery
    , getTrainingQuery
    , getValidationQuery
    , trainModel
    ) where

import Control.Monad (forM_, replicateM, when)
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (hClose)
import Text.Megaparsec (Parsec, parse, between, sepBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

import ProcessTools (ReadWrite(..), runProcessCreation_, withPipe, withProcess)

import Core
import Model.Stats (UnknownSet(..), ModelStats(..))
import Predictor (byteStringToModel)
import Utils.PropValue (PropValue(..))
import Query
import RuntimeData (getModelScript)
import Schema
import Query.Train
    ( QueryMode(..)
    , StepInfo(..)
    , StepInfoConfig(..)
    , TrainStepConfig(..)
    , trainStepQuery
    )
import qualified Sql.Transaction as SqlTrans
import TrainConfig

data ModelDescription = ModelDesc
    { modelName :: Text
    , modelPrettyName :: Maybe Text
    , modelDescription :: Maybe Text
    , modelTrainConfig :: TrainStepConfig
    }

splitQuery
    :: MonadQuery m => TrainingConfig -> m (Query StepInfo, Query StepInfo)
splitQuery (TrainConfig cfg) =
    return (trainStepQuery trainConfig, trainStepQuery validateConfig)
  where
    trainConfig = cfg { trainStepQueryMode = Train }
    validateConfig = cfg { trainStepQueryMode = Validate }

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
getTotalQuery (TrainConfig cfg) = trainStepQuery cfg{trainStepQueryMode = All}

getTotalQuery (LegacyTrainConfig LegacyConfig{..}) =
    trainStepQuery TrainStepConfig
        { trainStepInfoConfig = legacyStepInfoConfig
        , trainStepQueryMode = All
        , trainStepDatasets = legacyDatasets
        , trainStepProps = legacyProps
        , trainStepSeed = legacySeed
        , ..
        }
  where
    trainStepGraphs, trainStepVariants, trainStepSteps :: Percentage
    trainStepGraphs = $$(validRational 1)
    trainStepVariants = $$(validRational 1)
    trainStepSteps = $$(validRational 1)

trainModel
    :: (MonadMask m, MonadQuery m)
    => ModelDescription -> m (Key PredictionModel, Model)
trainModel ModelDesc{..} = do
    numEntries <- runSqlQueryCount trainQuery

    (propCount, propIds) <- runSqlQueryConduit trainQuery C.head >>= \case
        Just (v, _) -> return $ (VS.length v, VS.map propValuePropId v)
        Nothing -> logThrowM . PatternFailed $
            "Unable to compute property count for model"

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

            when (propCount /= VS.length featureImportance) $ do
                logThrowM $ UnexpectedMissingData
                    "Incorrect number of property importances reported by"
                    "model.py"

            let mkIndexMap idx key v = M.singleton key (idx, v)
                modelPropImportance = mconcat $
                    zipWith3 mkIndexMap [0..] (toSqlKey <$> VS.toList propIds)
                                              (VS.toList featureImportance)

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
            , predictionModelTrainGraphs = trainStepGraphs
            , predictionModelTrainVariants = trainStepVariants
            , predictionModelTrainSteps = trainStepSteps
            , predictionModelTrainSeed = trainStepSeed
            , predictionModelTotalUnknownCount = modelUnknownCount
            , predictionModelTimestamp = stepInfoTimestamp
            , predictionModelAllowNewer = stepInfoAllowNewer
            }

        forM_ (fromMaybe mempty trainStepDatasets) $
            SqlTrans.insert_ . ModelTrainDataset modelId

        forM_ (M.toList modelPropImportance) $ \(propId, (idx, v)) ->
            SqlTrans.insert_ $ ModelProperty modelId propId idx v

        forM_ (M.toList modelUnknownPreds) $ \(setId, UnknownSet{..}) -> do
            unknownId <- SqlTrans.insert $
                UnknownPrediction modelId stepInfoAlgorithm setId unknownSetOccurence

            forM_ (S.toList unknownSetImpls) $ \i -> do
                implKey <- SqlTrans.validateKey (fromSqlKey i)
                SqlTrans.insert_ $
                    UnknownPredictionSet unknownId implKey stepInfoAlgorithm

        return modelId

    return (modelId, model)
  where
    TrainStepConfig{..} = modelTrainConfig
    StepInfoConfig{..} = trainStepInfoConfig

    trainQuery = reduceInfo <$> trainStepQuery modelTrainConfig

    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

putProps :: Vector PropValue -> ByteString
putProps = LBS.toStrict . runPut . VS.mapM_ (putDoublehost . propValueValue)

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
