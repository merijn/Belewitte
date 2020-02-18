{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Data.Bifunctor (first)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Core
import Evaluate (evaluateModel, compareImplementations, percent)
import FormattedOutput (renderEntity, renderOutput)
import InteractiveInput
import Model
import ModelOptions
import Schema
import qualified Sql
import Train
import Validate

reportModelStats :: ModelStats -> ConduitT () Text SqlM ()
reportModelStats ModelStats{..} = do
    C.yield "Feature importance:\n"
    C.yieldMany sortedFeatures .| C.map renderFeature
    C.yield "\n"
    C.yield "Unknown predictions:\n"
    C.yieldMany sortedUnknown .| C.mapM renderImplSet
  where
    renderFeature :: (Text, Double) -> Text
    renderFeature (lbl, val) = mconcat
        [ padName (lbl <> ":"), " ", paddedPercent, "\n" ]
      where
        valTxt = percent val 1
        paddedPercent = T.replicate (6 - T.length valTxt) " " <> valTxt
        padName t = t <> T.replicate (maxFeatureNameLength - T.length t) " "

    renderImplSet :: UnknownSet -> SqlM Text
    renderImplSet UnknownSet{..} = do
        names <- foldMap wrap <$> traverse getName (S.toList unknownSetImpls)
        pure $ percent unknownSetOccurence modelUnknownCount <> " :\n" <> names
      where
        wrap :: Text -> Text
        wrap t = "    " <> t <> "\n"

        getName :: Int64 -> SqlM Text
        getName i = getImplName <$> Sql.getJust (toSqlKey i)

    maxFeatureNameLength :: Int
    maxFeatureNameLength = maximum $ map ((1+) . T.length . fst) features

    features :: [(Text, Double)]
    features = map (first ("Graph: " <>)) (M.toList modelGraphPropImportance)
            ++ map (first ("Step:  " <>)) (M.toList modelStepPropImportance)

    sortedFeatures :: [(Text, Double)]
    sortedFeatures = sortBy (flip (comparing snd)) features

    sortedUnknown :: [UnknownSet]
    sortedUnknown = sortBy (flip (comparing unknownSetOccurence)) modelUnknownPreds

setPlatformAndDatasets
    :: Key Platform -> Set (Key Dataset) -> TrainingConfig -> TrainingConfig
setPlatformAndDatasets platformId datasets trainConfig = case trainConfig of
    TrainConfig cfg@StepInfoConfig{stepInfoDatasets} -> TrainConfig cfg
      { stepInfoPlatform = platformId
      , stepInfoDatasets = updateDatasets stepInfoDatasets
      }
    LegacyTrainConfig cfg@LegacyConfig{legacyDatasets} -> LegacyTrainConfig cfg
      { legacyPlatform = platformId
      , legacyDatasets = updateDatasets legacyDatasets
      }
  where
    updateDatasets
        | S.null datasets = id
        | otherwise = const datasets

main :: IO ()
main = runSqlM commands $ \case
    Train{getConfig} -> runInput $ do
        modelName <- getInteractive textInput "Model Name"
        modelPrettyName <- getInteractive optionalInput "Model Pretty Name"

        desc <- T.unlines <$> getManyInteractive textInput "Model Description"
        let modelDescription
                | T.null desc = Nothing
                | otherwise = Just desc

        modelTrainConfig <- lift $ getConfig

        modelId <- lift $ fst <$> trainModel ModelDesc{..}

        liftIO $ print (fromSqlKey modelId)

    QueryModel{getModelEntity} -> do
        modelEnt@(Entity modelId _) <- getModelEntity
        modelStats <- getModelStats modelId
        renderOutput $ do
            C.yield $ renderEntity modelEnt
            C.yield "\n"
            reportModelStats modelStats

    ListModels{listModels} -> listModels

    Validate{getPlatformId,getModel,getDatasetIds} -> do
        platformId <- getPlatformId
        (modelId, model) <- getModel
        datasets <- getDatasetIds

        validationConfig <- setPlatformAndDatasets platformId datasets <$>
            getModelTrainingConfig modelId

        validateModel model validationConfig

    Evaluate{getPlatformId,getModel,defaultImpl,evaluateConfig,getDatasetIds} -> do
        platformId <- getPlatformId
        (modelId, model) <- getModel
        datasets <- getDatasetIds

        evalConfig <- setPlatformAndDatasets platformId datasets <$>
            getModelTrainingConfig modelId

        evaluateModel defaultImpl evaluateConfig model evalConfig

    Compare{getVariantInfoConfig,compareConfig} -> do
        variantInfoConfig <- getVariantInfoConfig
        compareImplementations variantInfoConfig compareConfig

    Export{getModel,cppFile} -> do
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId

        let (algoId, graphProps, stepProps) = case trainConfig of
                LegacyTrainConfig LegacyConfig{..} ->
                    (legacyAlgorithm, legacyGraphProps, legacyStepProps)
                TrainConfig StepInfoConfig{..} ->
                    (stepInfoAlgorithm, stepInfoGraphProps, stepInfoStepProps)

        impls <- Sql.queryImplementations algoId

        dumpCppModel cppFile model graphProps stepProps
            (implementationName <$> impls)
