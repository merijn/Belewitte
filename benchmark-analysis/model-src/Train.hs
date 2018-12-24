{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Train
    ( ModelStats(..)
    , TrainingConfig(..)
    , getTotalQuery
    , getTrainingQuery
    , getValidationQuery
    , getModelTrainingConfig
    , getModelStats
    , trainModel
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Catch (mask_, throwM)
import Control.Monad.Trans.Resource (register, release)
import Data.Binary.Get (getDoublehost, getRemainingLazyByteString, runGet)
import Data.Binary.Put (putDoublehost, putInt64host, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process
    (CreateProcess, proc, withCheckedProcessCleanup)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Database.Persist.Sqlite ((==.))
import qualified Database.Persist.Sqlite as Sql
import System.IO (hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Types (Fd)
import Text.Megaparsec (Parsec, parse, between, sepBy1, some)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

import Core
import Model
import Paths_benchmark_analysis (getDataFileName)
import Query
import Schema

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

splitQuery
    :: Key GPU -> TrainingConfig -> SqlM (Query StepInfo, Query StepInfo)
splitQuery gpuId cfg@TrainConfig{..} = do
    rowCount <- runSqlQueryCount query

    let trainingSize :: Integer
        trainingSize = round (fromIntegral rowCount * trainFraction)

    return $ randomizeQuery trainSeed trainingSize query
  where
    query = getTotalQuery gpuId cfg

getTrainingQuery :: Key GPU -> TrainingConfig -> SqlM (Query StepInfo)
getTrainingQuery gpuId = fmap fst . splitQuery gpuId

getValidationQuery :: Key GPU -> TrainingConfig -> SqlM (Query StepInfo)
getValidationQuery gpuId = fmap snd . splitQuery gpuId

getTotalQuery :: Key GPU -> TrainingConfig -> Query StepInfo
getTotalQuery gpuId TrainConfig{..} =
  stepInfoQuery gpuId trainGraphProps trainStepProps

getModelTrainingConfig :: Key PredictionModel -> SqlM TrainingConfig
getModelTrainingConfig modelId = do
    PredictionModel{..} <- Sql.getJust modelId

    graphProps <- runConduitRes $
        Sql.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.map (modelGraphPropertyProperty . Sql.entityVal)
        .| C.foldMap S.singleton

    stepProps <- runConduitRes $
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

    modelGraphPropImportance <- runConduitRes $
        Sql.selectSource [ModelGraphPropertyModelId ==. modelId] []
        .| C.foldMap (graphPropMap . Sql.entityVal)

    modelStepPropImportance <- runConduitRes $
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

trainModel :: Key GPU -> TrainingConfig -> SqlM (Key PredictionModel, Model)
trainModel gpuId trainCfg@TrainConfig{..} = do
    trainQuery <- fmap reduceInfo <$> getTrainingQuery gpuId trainCfg
    numEntries <- runSqlQueryCount trainQuery

    Just propCount <- fmap (VU.length . fst) <$> runSqlQuery trainQuery C.head

    scriptProc <- liftIO $
        proc <$> getDataFileName "runtime-data/scripts/model.py"

    let process :: Fd -> CreateProcess
        process fd = scriptProc
            [ "--entries"
            , show numEntries
            , "--properties"
            , show propCount
            , "--fd"
            , show fd
            ]

    ((outFd, closeOut), (resultsHnd, closeResults)) <- mask_ $ do
        (fdOut, fdIn) <- liftIO createPipe
        hndIn <- liftIO $ fdToHandle fdIn
        closeIn <- register $ hClose hndIn
        closeOut <- register $ closeFd fdOut
        return ((fdOut, closeOut), (hndIn, closeIn))

    let handleStreams (propSink, closeSink) modelHnd errHnd = do
            register $ hClose modelHnd
            release closeOut
            processIn <- register closeSink

            let propertySink = ZipSink $ do
                    C.map (putProps . fst) .| propSink
                    release processIn

                resultSink = ZipSink $ do
                    C.map (putResults . snd) .| C.sinkHandle resultsHnd
                    release closeResults

                combinedSink = getZipSink (propertySink *> resultSink)

            runSqlQuery trainQuery combinedSink

            (featureImportance, model) <- liftIO $
                getResult propCount <$> BS.hGetContents modelHnd

            unknownTxt <- liftIO $ T.hGetContents errHnd
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

    timestamp <- liftIO getCurrentTime
    (model, ModelStats{..}) <-
        withCheckedProcessCleanup (process outFd) handleStreams

    modelId <- Sql.insert $
        PredictionModel gpuId model trainFraction trainSeed modelUnknownCount
                        timestamp

    forM_ (M.toList modelGraphPropImportance) $
        Sql.insert_ . uncurry (ModelGraphProperty modelId)

    forM_ (M.toList modelStepPropImportance) $
        Sql.insert_ . uncurry (ModelStepProperty modelId)

    forM_ modelUnknownPreds $ \(count, impls) -> do
        unknownId <- Sql.insert $ UnknownPrediction modelId count
        forM_ impls $ Sql.insert_ . UnknownSet unknownId . toSqlKey

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

getUnknowns :: MonadThrow m => Text -> m (Int, [(Int, Set Int64)])
getUnknowns txt = case parse parseUnknowns "getUnknowns" txt of
    Left e -> throwM e
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
