{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Train
    ( ModelDescription(..)
    , ModelStats(..)
    , TrainingConfig(..)
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
import qualified Sql

data ModelStats = ModelStats
     { modelGraphPropImportance :: Map Text Double
     , modelStepPropImportance :: Map Text Double
     , modelUnknownCount :: Int
     , modelUnknownPreds :: [(Int, Set Int64)]
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
    :: (MonadResource m, MonadSql m) => Key PredictionModel -> m TrainingConfig
getModelTrainingConfig modelId = do
    PredictionModel{..} <- Sql.getJust modelId

    graphProps <- runConduit $
        Sql.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.map (modelGraphPropertyProperty . Sql.entityVal)
        .| C.foldMap S.singleton

    stepProps <- runConduit $
        Sql.selectSource [ModelStepPropertyModelId ==. modelId] []
        .| C.map (modelStepPropertyProperty . Sql.entityVal)
        .| C.foldMap S.singleton

    return TrainConfig
        { trainGraphProps = graphProps
        , trainStepProps = stepProps
        , trainFraction = predictionModelTrainFraction
        , trainSeed = predictionModelTrainSeed
        }

getModelStats :: Key PredictionModel -> SqlM ModelStats
getModelStats modelId = do
    modelUnknownCount <-
        predictionModelTotalUnknownCount <$> Sql.getJust modelId

    modelGraphPropImportance <- runConduit $
        Sql.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.foldMap (graphPropMap . Sql.entityVal)

    modelStepPropImportance <- runConduit $
        Sql.selectSource [ModelStepPropertyModelId ==. modelId] []
        .| C.foldMap (stepPropMap . Sql.entityVal)

    unknowns <- Sql.selectList [UnknownPredictionModelId ==. modelId] []
    modelUnknownPreds <- forM unknowns $ \Sql.Entity{..} -> do
        let UnknownPrediction{unknownPredictionCount} = entityVal

        implSet <- runConduitRes $
            Sql.selectSource [UnknownSetUnknownPredId ==. entityKey] []
            .| C.foldMap (toImplSet . Sql.entityVal)

        return (unknownPredictionCount, implSet)

    return ModelStats{..}
  where
    graphPropMap ModelGraphProperty{..} =
      M.singleton modelGraphPropertyProperty modelGraphPropertyImportance

    stepPropMap ModelStepProperty{..} =
      M.singleton modelStepPropertyProperty modelStepPropertyImportance

    toImplSet UnknownSet{..} = S.singleton $ fromSqlKey unknownSetImplId

trainModel
    :: (MonadMask m, MonadQuery m, MonadTagFail m)
    => Key Algorithm
    -> Key Platform
    -> ModelDescription
    -> m (Key PredictionModel, Model)
trainModel algoId platId ModelDesc{..} = do
    let TrainConfig{..} = modelTrainConfig
    trainQuery <- fmap reduceInfo <$>
        getTrainingQuery algoId platId modelTrainConfig

    numEntries <- runSqlQueryCount trainQuery

    Just propCount <- logIfFail "Unable to compute property count" empty $
        fmap (VU.length . fst) <$> runSqlQueryConduit trainQuery C.head

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

    modelId <- Sql.insert $ PredictionModel
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
        Sql.insert_ . uncurry (ModelGraphProperty modelId)

    forM_ (M.toList modelStepPropImportance) $
        Sql.insert_ . uncurry (ModelStepProperty modelId)

    forM_ modelUnknownPreds $ \(count, impls) -> do
        unknownId <- Sql.insert $ UnknownPrediction modelId algoId count
        forM_ impls $ \impl ->
            Sql.insert_ $ UnknownSet unknownId (toSqlKey impl) algoId

    return (modelId, model)
  where
    reduceInfo StepInfo{..} = (stepProps, stepBestImpl)

    empty :: String
    empty = ""

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
    :: (MonadLogger m, MonadThrow m) => Text -> m (Int, [(Int, Set Int64)])
getUnknowns txt = case parse parseUnknowns "getUnknowns" txt of
    Left e -> logThrowM . ModelInfoParseFailed . T.pack $ errorBundlePretty e
    Right r -> return r

parseUnknowns :: Parsec Void Text (Int, [(Int, Set Int64)])
parseUnknowns = (,) <$> decimal <* eol <*> some parseUnknown

parseUnknown :: Parsec Void Text (Int, Set Int64)
parseUnknown = do
    count <- decimal
    string " : "
    impls <- between (char '(') (char ')') $
        S.fromList <$> sepBy1 decimal (string ", ")
    eol
    return (count, impls)
