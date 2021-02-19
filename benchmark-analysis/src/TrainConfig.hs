{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module TrainConfig
    ( LegacyConfig(..)
    , TrainingConfig(..)
    , getModelTrainingConfig
    , getStepInfoConfig
    , setTrainingConfigAllowNewer
    , setTrainingConfigDatasets
    , setTrainingConfigPlatform
    , setTrainingConfigSkipIncomplete
    , setTrainingConfigTimestamp
    ) where

import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C
import Data.Monoid (Endo(..))
import Data.Set (Set)
import qualified Data.Set as S

import Core
import Query.Train (QueryMode(All), StepInfoConfig(..), TrainStepConfig(..))
import Schema
import Sql.Transaction (MonadSql, (==.))
import qualified Sql.Transaction as SqlTrans

data TrainingConfig
    = TrainConfig TrainStepConfig
    | LegacyTrainConfig LegacyConfig
    deriving (Show)

data LegacyConfig = LegacyConfig
    { legacyStepInfoConfig :: StepInfoConfig
    , legacyProps :: Set (Key PropertyName)
    , legacyFraction :: Double
    , legacySeed :: Int64
    , legacyDatasets :: Maybe (Set (Key Dataset))
    } deriving (Show)

getStepInfoConfig :: TrainingConfig -> StepInfoConfig
getStepInfoConfig trainConfig = case trainConfig of
    TrainConfig cfg -> trainStepInfoConfig cfg
    LegacyTrainConfig cfg -> legacyStepInfoConfig cfg

mapStepInfoConfig
    :: (StepInfoConfig -> StepInfoConfig) -> TrainingConfig -> TrainingConfig
mapStepInfoConfig f = \case
    TrainConfig cfg -> TrainConfig cfg
        {trainStepInfoConfig = f (trainStepInfoConfig cfg)}
    LegacyTrainConfig cfg -> LegacyTrainConfig cfg
        {legacyStepInfoConfig = f (legacyStepInfoConfig cfg)}

setTrainingConfigAllowNewer :: AllowNewer -> Endo TrainingConfig
setTrainingConfigAllowNewer val = Endo . mapStepInfoConfig $ \stepCfg ->
    stepCfg { stepInfoAllowNewer = val }

setTrainingConfigDatasets
    :: Maybe (Set (Key Dataset)) -> Endo TrainingConfig
setTrainingConfigDatasets datasets = Endo $ \case
    TrainConfig cfg@TrainStepConfig{trainStepDatasets} ->
        TrainConfig cfg{ trainStepDatasets = updateDatasets trainStepDatasets }

    LegacyTrainConfig cfg@LegacyConfig{legacyDatasets} -> LegacyTrainConfig cfg
      { legacyDatasets = updateDatasets legacyDatasets }
  where
    updateDatasets
        | maybe True S.null datasets = id
        | otherwise = const datasets

setTrainingConfigPlatform :: Key Platform -> Endo TrainingConfig
setTrainingConfigPlatform platformId = Endo . mapStepInfoConfig $ \cfg ->
    cfg{ stepInfoPlatform = platformId }

setTrainingConfigSkipIncomplete :: Bool -> Endo TrainingConfig
setTrainingConfigSkipIncomplete val = Endo $ \trainConfig ->
    case trainConfig of
        LegacyTrainConfig{} -> trainConfig
        TrainConfig{} -> mapStepInfoConfig go trainConfig
  where
    go cfg = cfg{ stepInfoFilterIncomplete = val }

setTrainingConfigTimestamp :: Maybe UTCTime -> Endo TrainingConfig
setTrainingConfigTimestamp val = Endo $ \trainConfig -> case trainConfig of
    LegacyTrainConfig{} -> trainConfig
    TrainConfig{} -> mapStepInfoConfig go trainConfig
  where
    go cfg = case val of
        Nothing -> cfg
        Just v -> cfg{ stepInfoTimestamp = v }

getModelTrainingConfig :: MonadSql m => Key PredictionModel -> m TrainingConfig
getModelTrainingConfig modelId = SqlTrans.runTransaction $ do
    PredictionModel{..} <- SqlTrans.getJust modelId

    modelProps <- SqlTrans.selectSource [ModelPropertyModelId ==. modelId] [] $
        C.foldMap (S.singleton . modelPropertyPropId . SqlTrans.entityVal)

    let checkKey :: Entity PropertyName -> Bool
        checkKey (Entity k _) = S.member k modelProps

    props <- SqlTrans.selectSource [] [] $
        C.filter checkKey .| C.foldMap (S.singleton . entityKey)

    trainingDatasets <-
        SqlTrans.selectSource [ModelTrainDatasetModelId ==. modelId] [] $
            C.map (modelTrainDatasetDatasetId . SqlTrans.entityVal)
            .| C.foldMap S.singleton

    if predictionModelLegacyTrainFraction /= 0.0
       then return $ LegacyTrainConfig LegacyConfig
                { legacyStepInfoConfig = StepInfoConfig
                  { stepInfoAlgorithm = predictionModelAlgorithmId
                  , stepInfoPlatform = predictionModelPlatformId
                  , stepInfoCommit = predictionModelAlgorithmVersion
                  , stepInfoFilterIncomplete = False
                  , stepInfoTimestamp = predictionModelTimestamp
                  , stepInfoAllowNewer = predictionModelAllowNewer
                  }
                , legacyProps = props
                , legacyFraction = predictionModelLegacyTrainFraction
                , legacySeed = predictionModelTrainSeed
                , legacyDatasets = Just trainingDatasets
                }
       else return $ TrainConfig TrainStepConfig
                { trainStepInfoConfig = StepInfoConfig
                  { stepInfoAlgorithm = predictionModelAlgorithmId
                  , stepInfoPlatform = predictionModelPlatformId
                  , stepInfoCommit = predictionModelAlgorithmVersion
                  , stepInfoFilterIncomplete = predictionModelSkipIncomplete
                  , stepInfoTimestamp = predictionModelTimestamp
                  , stepInfoAllowNewer = predictionModelAllowNewer
                  }
                , trainStepQueryMode = All
                , trainStepDatasets = Just trainingDatasets
                , trainStepProps = props
                , trainStepSeed = predictionModelTrainSeed
                , trainStepGraphs = predictionModelTrainGraphs
                , trainStepVariants = predictionModelTrainVariants
                , trainStepSteps = predictionModelTrainSteps
                }
