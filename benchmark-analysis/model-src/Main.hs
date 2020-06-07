{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Control.Monad (forM, unless)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Core
import Evaluate (evaluateModel, compareImplementations)
import FormattedOutput (renderEntity, renderOutput)
import InteractiveInput
import Model
import Model.Stats (ModelStats(..), UnknownSet(..), getModelStats)
import ModelOptions
import Predictor
import Schema
import Sql ((==.))
import qualified Sql
import Train
import Validate

reportModelStats :: ModelStats -> ConduitT () Text SqlM ()
reportModelStats ModelStats{..} = do
    features <- forM (M.toList modelPropImportance) $ \(propId, (_, val)) -> do
        PropertyName{..} <- Sql.getJust propId

        let label | propertyNameIsStepProp = "Step:  "
                  | otherwise = "Graph: "

        return (label <> propertyNameProperty, val)

    let maxFeatureNameLength :: Int
        maxFeatureNameLength = maximum $ map ((1+) . T.length . fst) features

        sortedFeatures :: [(Text, Double)]
        sortedFeatures = sortBy (flip (comparing snd)) features

    C.yield "Feature importance:\n"
    C.yieldMany sortedFeatures .| C.map (renderFeature maxFeatureNameLength)
    unless (null sortedUnknown) $ do
        C.yield "\n"
        C.yield "Unknown predictions:\n"
        C.yieldMany sortedUnknown .| C.mapM renderImplSet
  where
    renderFeature :: Int -> (Text, Double) -> Text
    renderFeature maxFeatureNameLength (lbl, val) = mconcat
        [ padName (lbl <> ":"), " " , paddedPercent, "\n" ]
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

        getName :: Key Implementation -> SqlM Text
        getName i = getImplName <$> Sql.getJust i

    sortedUnknown :: [UnknownSet]
    sortedUnknown = sortBy (flip (comparing unknownSetOccurence))
                  $ M.elems modelUnknownPreds

main :: IO ()
main = runCommand commands $ \case
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

    ValidateModel{getModelId,getOptionalPlatformId,getOptionalDatasetIds} -> do
        modelId <- getModelId
        platformId <- getOptionalPlatformId
        datasets <- getOptionalDatasetIds
        predictor <- predictorFromModelId modelId

        validationConfig <- getModelTrainingConfig modelId

        validateModel predictor validationConfig platformId datasets

    EvaluatePredictor
      { getPlatformId
      , getPredictorConfigs
      , shouldFilterIncomplete
      , evaluateConfig
      , getDatasetIds
      } -> do
        let setSkip = setTrainingConfigSkipIncomplete shouldFilterIncomplete

        ~predictors@(basePred:_) <- getPredictorConfigs >>= mapM loadPredictor

        setPlatformId <- setTrainingConfigPlatform <$> getPlatformId
        setDatasets <- setTrainingConfigDatasets <$> getDatasetIds
        evalConfig <- setSkip . setPlatformId . setDatasets <$>
            getModelTrainingConfig (rawPredictorId basePred)

        evaluateModel predictors evaluateConfig evalConfig

    Compare{getVariantInfoConfig,compareConfig} -> do
        variantInfoConfig <- getVariantInfoConfig
        compareImplementations variantInfoConfig compareConfig

    ExportModel{getModel,cppFile} -> do
        Entity modelId PredictionModel{..} <- getModel

        impls <- Sql.queryImplementations predictionModelAlgorithmId
        modelProps <- Sql.selectSource [ModelPropertyModelId ==. modelId] [] $
            C.mapM (mkPropIdx . entityVal) .| C.foldMap S.singleton

        dumpCppModel cppFile predictionModelModel modelProps
            (implementationName <$> impls)
  where
    mkPropIdx ModelProperty{..} = do
        name <- propertyNameProperty <$> Sql.getJust modelPropertyPropId
        return (modelPropertyPropertyIdx, name)
