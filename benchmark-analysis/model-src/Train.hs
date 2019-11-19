{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Train
    ( ModelDescription(..)
    , ModelStats(..)
    , TrainingConfig(..)
    , UnknownSet (..)
    , getTotalQuery
    , getTrainingQuery
    , getValidationQuery
    , getModelTrainingConfig
    , getModelStats
    , trainModel
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Resource (register, release)
import Data.Binary.Get (getDoublehost, getRemainingLazyByteString, runGet)
import Data.Binary.Put (putDoublehost, putInt64host, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import System.IO (hClose)
import Text.Megaparsec (Parsec, parse, between, sepBy1, some)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

import Core
import Model
import Utils.Process (ReadWrite(..), runProcessCreation_, withPipe, withProcess)
import Query
import RuntimeData (getModelScript)
import Schema
import StepQuery (StepInfo(..), stepInfoQuery)
import Sql (MonadSql, (==.))
import qualified Sql.Transaction as SqlTrans

data UnknownSet = UnknownSet
    { unknownSetId :: Int64
    , unknownSetOccurence :: Int
    , unknownSetImpls :: Set Int64
    } deriving (Eq, Show)

data ModelStats = ModelStats
    { modelGraphPropImportance :: Map Text Double
    , modelStepPropImportance :: Map Text Double
    , modelUnknownCount :: Int
    , modelUnknownPreds :: [UnknownSet]
    } deriving (Eq, Show)

data TrainingConfig = TrainConfig
    { trainGraphProps :: Set Text
    , trainStepProps :: Set Text
    , trainFraction :: Double
    , trainSeed :: Int
    }

data ModelDescription = ModelDesc
    { modelName :: Text
    , modelPrettyName :: Maybe Text
    , modelDescription :: Maybe Text
    , modelTrainConfig :: TrainingConfig
    }

splitQuery
    :: MonadQuery m
    => Key Algorithm
    -> Key Platform
    -> TrainingConfig
    -> m (Query StepInfo, Query StepInfo)
splitQuery algoId platformId cfg@TrainConfig{..} = do
    rowCount <- runSqlQueryCount query

    let trainingSize :: Int
        trainingSize = round (fromIntegral rowCount * trainFraction)

    return $ randomizeQuery trainSeed trainingSize query
  where
    query = getTotalQuery algoId platformId cfg

getTrainingQuery
    :: MonadQuery m
    => Key Algorithm -> Key Platform -> TrainingConfig -> m (Query StepInfo)
getTrainingQuery algoId platformId = fmap fst . splitQuery algoId platformId

getValidationQuery
    :: MonadQuery m
    => Key Algorithm -> Key Platform -> TrainingConfig -> m (Query StepInfo)
getValidationQuery algoId platformId = fmap snd . splitQuery algoId platformId

getTotalQuery
    :: Key Algorithm -> Key Platform -> TrainingConfig -> Query StepInfo
getTotalQuery algoId platformId TrainConfig{..} =
  stepInfoQuery algoId platformId trainGraphProps trainStepProps

getModelTrainingConfig
    :: MonadSql m => Key PredictionModel -> m TrainingConfig
getModelTrainingConfig modelId = SqlTrans.runTransaction $ do
    PredictionModel{..} <- SqlTrans.getJust modelId

    graphProps <- runConduit $
        SqlTrans.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.map (modelGraphPropertyProperty . SqlTrans.entityVal)
        .| C.foldMap S.singleton

    stepProps <- runConduit $
        SqlTrans.selectSource [ModelStepPropertyModelId ==. modelId] []
        .| C.map (modelStepPropertyProperty . SqlTrans.entityVal)
        .| C.foldMap S.singleton

    return TrainConfig
        { trainGraphProps = graphProps
        , trainStepProps = stepProps
        , trainFraction = predictionModelTrainFraction
        , trainSeed = predictionModelTrainSeed
        }

getModelStats :: Key PredictionModel -> SqlM ModelStats
getModelStats modelId = SqlTrans.runTransaction $ do
    modelUnknownCount <-
        predictionModelTotalUnknownCount <$> SqlTrans.getJust modelId

    modelGraphPropImportance <- runConduit $
        SqlTrans.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.foldMap (graphPropMap . SqlTrans.entityVal)

    modelStepPropImportance <- runConduit $
        SqlTrans.selectSource [ModelStepPropertyModelId ==. modelId] []
        .| C.foldMap (stepPropMap . SqlTrans.entityVal)

    unknowns <- SqlTrans.selectList [UnknownPredictionModelId ==. modelId] []
    modelUnknownPreds <- forM unknowns $ \SqlTrans.Entity{..} -> do
        let UnknownPrediction{..} = entityVal
            filters = [UnknownPredictionSetUnknownPredId ==. entityKey]

        implSet <- runConduit $
            SqlTrans.selectSource filters []
            .| C.foldMap (toImplSet . SqlTrans.entityVal)

        return $ UnknownSet unknownPredictionUnknownSetId unknownPredictionCount implSet

    return ModelStats{..}
  where
    graphPropMap ModelGraphProperty{..} =
      M.singleton modelGraphPropertyProperty modelGraphPropertyImportance

    stepPropMap ModelStepProperty{..} =
      M.singleton modelStepPropertyProperty modelStepPropertyImportance

    toImplSet UnknownPredictionSet{..} =
      S.singleton $ fromSqlKey unknownPredictionSetImplId

trainModel
    :: (MonadMask m, MonadQuery m)
    => Key Algorithm
    -> Key Platform
    -> ModelDescription
    -> m (Key PredictionModel, Model)
trainModel algoId platId ModelDesc{..} = do
    let TrainConfig{..} = modelTrainConfig
    trainQuery <- fmap reduceInfo <$>
        getTrainingQuery algoId platId modelTrainConfig

    numEntries <- runSqlQueryCount trainQuery

    propCount <- runSqlQueryConduit trainQuery C.head >>= \case
        Just (v, _) -> return $ VU.length v
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

            let (graphProps, stepProps) = VU.splitAt (S.size trainGraphProps)
                                                     featureImportance

                modelGraphPropImportance :: Map Text Double
                modelGraphPropImportance = M.fromList $
                    zip (S.toAscList trainGraphProps)
                        (VU.toList graphProps)

                modelStepPropImportance :: Map Text Double
                modelStepPropImportance = M.fromList $
                    zip (S.toAscList trainStepProps)
                        (VU.toList stepProps)

            return (model, ModelStats{..})

    modelId <- SqlTrans.runTransaction $ do
        modelId <- SqlTrans.insert $ PredictionModel
            { predictionModelPlatformId = platId
            , predictionModelAlgorithmId = algoId
            , predictionModelName = modelName
            , predictionModelPrettyName = modelPrettyName
            , predictionModelDescription = modelDescription
            , predictionModelModel = model
            , predictionModelTrainFraction = trainFraction
            , predictionModelTrainSeed = trainSeed
            , predictionModelTotalUnknownCount = modelUnknownCount
            , predictionModelTimestamp = timestamp
            }

        forM_ (M.toList modelGraphPropImportance) $
            SqlTrans.insert_ . uncurry (ModelGraphProperty modelId)

        forM_ (M.toList modelStepPropImportance) $
            SqlTrans.insert_ . uncurry (ModelStepProperty modelId)

        forM_ modelUnknownPreds $ \UnknownSet{..} -> do
            unknownId <- SqlTrans.insert $
                UnknownPrediction modelId algoId unknownSetId unknownSetOccurence

            forM_ unknownSetImpls $ \impl -> do
                implKey <- SqlTrans.validateKey "Implementation" impl
                SqlTrans.insert_ $ UnknownPredictionSet unknownId implKey algoId

        return modelId

    return (modelId, model)
  where
    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

putProps :: Vector Double -> ByteString
putProps = LBS.toStrict . runPut . VU.mapM_ putDoublehost

putResults :: Int64 -> ByteString
putResults = LBS.toStrict . runPut . putInt64host

getResult :: Int -> ByteString -> (Vector Double, Model)
getResult columnCount = runGet parseBlock . LBS.fromStrict
  where
    parseBlock = do
        dbls <- VU.replicateM columnCount getDoublehost
        bs <- LBS.toStrict <$> getRemainingLazyByteString
        return (dbls, byteStringToModel bs)

getUnknowns
    :: (MonadLogger m, MonadThrow m) => Text -> m (Int, [UnknownSet])
getUnknowns txt = case parse parseUnknowns "getUnknowns" txt of
    Left e -> logThrowM . ModelInfoParseFailed . T.pack $ errorBundlePretty e
    Right r -> return r

parseUnknowns :: Parsec Void Text (Int, [UnknownSet])
parseUnknowns = (,) <$> decimal <* eol <*> some parseUnknown

parseUnknown :: Parsec Void Text UnknownSet
parseUnknown = do
    setId <- decimal <* string " : "
    impls <- between (char '(') (char ')') $
        S.fromList <$> sepBy1 decimal (string ", ")
    string " : "
    count <- decimal
    eol
    return $ UnknownSet setId count impls
