{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Data.Bifunctor (first)
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Core
import Evaluate (evaluateModel, compareImplementations, percent)
import FormattedOutput (renderOutput)
import InteractiveInput
import Model
import ModelOptions
import Schema
import qualified Sql
import Train
import Validate

reportModelStats :: ModelStats -> SqlM ()
reportModelStats ModelStats{..} = renderOutput $ do
    C.yieldMany sortedFeatures .| C.map renderFeature
    C.yield "\n"
    C.yieldMany sortedUnknown .| C.mapM renderImplSet
  where
    renderFeature :: (Text, Double) -> Text
    renderFeature (lbl, val) = lbl <> ": " <> percent val 1 <> "\n"

    renderImplSet :: UnknownSet -> SqlM Text
    renderImplSet UnknownSet{..} = do
        names <- foldMap wrap <$> traverse getName (S.toList unknownSetImpls)
        return $ percent unknownSetOccurence modelUnknownCount <> " :\n" <> names
      where
        wrap :: Text -> Text
        wrap t = "    " <> t <> "\n"

        getName :: Int64 -> SqlM Text
        getName i = getImplName <$> Sql.getJust (toSqlKey i)

    features :: [(Text, Double)]
    features = map (first ("Graph:" <>)) (M.toList modelGraphPropImportance)
            ++ map (first ("Step:" <>)) (M.toList modelStepPropImportance)

    sortedFeatures :: [(Text, Double)]
    sortedFeatures = sortBy (flip (comparing snd)) features

    sortedUnknown :: [UnknownSet]
    sortedUnknown = sortBy (flip (comparing unknownSetOccurence)) modelUnknownPreds

main :: IO ()
main = runSqlM commands $ \case
    Train{getConfig} -> runInput $ do
        let algoInput = sqlInput AlgorithmName UniqAlgorithm
            platformInput = sqlInput PlatformName UniqPlatform

        Entity algoId _ <- getInteractive algoInput "Algorithm Name"
        Entity platformId _ <- getInteractive platformInput "Platform Name"

        modelName <- getInteractive textInput "Model Name"
        modelPrettyName <- getInteractive optionalInput "Model Pretty Name"

        desc <- T.unlines <$> getManyInteractive textInput "Model Description"
        let modelDescription
                | T.null desc = Nothing
                | otherwise = Just desc

        modelTrainConfig <- lift $ getConfig algoId

        modelId <- lift $ fst <$> trainModel algoId platformId ModelDesc{..}

        liftIO $ print (fromSqlKey modelId)

    QueryModel{getModel} -> do
        (_, modelId, _) <- getModel
        getModelStats modelId >>= reportModelStats

    Validate{getPlatformId,getModel} -> do
        platformId <- getPlatformId
        (algoId, modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        validateModel algoId platformId model trainConfig

    Evaluate{getPlatformId,getModel,defaultImpl,evaluateConfig} -> do
        platId <- getPlatformId
        (algoId, modelId, model) <- getModel
        algo <- Sql.getJustEntity algoId
        trainConfig <- getModelTrainingConfig modelId
        evaluateModel algo platId defaultImpl evaluateConfig model trainConfig

    Compare{getAlgoId,getPlatformId,getCommit,getDatasetId,compareConfig} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        commit <- getCommit
        datasetId <- sequence getDatasetId
        compareImplementations algoId platformId commit datasetId compareConfig

    Export{getModel,cppFile} -> do
        (algoId, modelId, model) <- getModel
        impls <- Sql.queryImplementations algoId
        trainConfig <- getModelTrainingConfig modelId

        let (graphProps, stepProps) = case trainConfig of
                LegacyTrainConfig LegacyConfig{..} ->
                    (legacyGraphProps, legacyStepProps)
                TrainConfig StepInfoConfig{..} ->
                    (stepInfoGraphProps, stepInfoStepProps)

        dumpCppModel cppFile model graphProps stepProps
            (implementationName <$> impls)
