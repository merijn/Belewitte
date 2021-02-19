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
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LT

import ProcessTools (withStdin)

import Core
import Evaluate (evaluateModel, compareImplementations)
import FormattedOutput (renderEntity, renderOutput)
import InteractiveInput
import Model.Stats (ModelStats(..), UnknownSet(..), getModelStats)
import ModelOptions
import Predictor
import PredictorResults (outputPredictorResults)
import RuntimeData (getCxxCompilerWrapper)
import Schema
import qualified Sql
import Train
import TrainConfig
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

exportPredictor :: ExportType -> Maybe FilePath -> PredictorConfig -> SqlM ()
exportPredictor exportType exportOutput predConfig = do
    predictor <- loadPredictor predConfig
    modelSrc <- predictorToCxx predictor

    let predName = rawPredictorName predictor

    case exportType of
        CppFile -> liftIO $ LT.writeFile (outputFile predName) modelSrc
        SharedLib -> do
            cxxWrapper <- getCxxCompilerWrapper (outputFile predName)
            withStdin cxxWrapper $ \hnd -> liftIO $ LT.hPutStr hnd modelSrc
  where
    outputFile :: Text -> FilePath
    outputFile modelName = case exportOutput of
        Just s -> s
        Nothing -> T.unpack . T.replace ":" "." $ modelName <> fileSuffix

    fileSuffix :: Text
    fileSuffix = case exportType of
        SharedLib -> ".so"
        CppFile -> ".cpp"

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
            renderEntity modelEnt >>= C.yield
            C.yield "\n"
            reportModelStats modelStats

    ListModels{listModels} -> listModels

    ValidateModel
      { getPredictorConfig
      , getOptionalPlatformId
      , getOptionalDatasetIds
      , optionalUTCTime
      } -> do
        predictor <- getPredictorConfig >>= loadPredictor
        platformId <- getOptionalPlatformId
        datasets <- getOptionalDatasetIds

        validationConfig <- getModelTrainingConfig (rawPredictorId predictor)

        validateModel predictor validationConfig optionalUTCTime platformId datasets

    EvaluatePredictor
      { getPlatformId
      , getPredictorConfig
      , shouldFilterIncomplete
      , allowNewer
      , evaluateConfig
      , getDatasetIds
      , optionalUTCTime
      } -> do
        predictor <- getPredictorConfig >>= loadPredictor
        mPlatformId <- getPlatformId

        let updateConfig = appEndo $ mconcat
                [ foldMap setTrainingConfigSkipIncomplete shouldFilterIncomplete
                , setTrainingConfigTimestamp optionalUTCTime
                , foldMap setTrainingConfigPlatform mPlatformId
                , foldMap setTrainingConfigAllowNewer allowNewer
                ]

        datasets <- fromMaybe S.empty <$> getDatasetIds
        evalConfig <- getStepInfoConfig . updateConfig <$>
            getModelTrainingConfig (rawPredictorId predictor)

        evaluateModel [predictor] evaluateConfig evalConfig datasets

    EvaluatePredictors
      { getPredictorConfigs
      , evaluateConfig
      , getStepConfig
      , getDatasets
      } -> do
        predictorCfgs@(pCfg:_) <- getPredictorConfigs
        algoId <- getPredictorConfigAlgorithmId pCfg

        predictors <- forM predictorCfgs $ \predCfg -> do
            algoId' <- getPredictorConfigAlgorithmId predCfg
            unless (algoId == algoId') $
                logThrowM . GenericInvariantViolation $ mconcat
                    [ "Found predictors for algorithm #", showSqlKey algoId
                    , " and #", showSqlKey algoId'
                    ]

            loadPredictor predCfg

        stpConfig <- getStepConfig algoId
        datasets <- getDatasets
        evaluateModel predictors evaluateConfig stpConfig datasets

    PredictionResults{getPredictionConfig} -> do
        getPredictionConfig >>= outputPredictorResults

    Compare{getVariantInfoConfig,compareConfig} -> do
        variantInfoConfig <- getVariantInfoConfig
        compareImplementations variantInfoConfig compareConfig

    PredictorExport{exportType,getPredictorConfig,exportOutput} -> do
        predictorConfig <- getPredictorConfig
        exportPredictor exportType exportOutput predictorConfig

    MultiPredictorExport{exportType,getPredictorConfigs} -> do
        predictorConfigs <- getPredictorConfigs
        mapM_ (exportPredictor exportType Nothing) predictorConfigs
