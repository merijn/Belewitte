{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Conduit (ConduitT, Void, (.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Core
import Evaluate (evaluateModel, compareImplementations, percent)
import FormattedOutput (renderOutput)
import InteractiveInput
import Model
import Options
import Query
import Schema
import Sql (queryImplementations, getJust)
import StepQuery (StepInfo, stepInfoQuery)
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

    renderImplSet :: (Int, Set Int64) -> SqlM Text
    renderImplSet (count, implSet) = do
        names <- foldMap wrap <$> traverse getName (S.toList implSet)
        return $ percent count modelUnknownCount <> " :\n" <> names
      where
        wrap :: Text -> Text
        wrap t = "    " <> t <> "\n"

        getName :: Int64 -> SqlM Text
        getName i = getImplName <$> getJust (toSqlKey i)

    features :: [(Text, Double)]
    features = map (first ("Graph:" <>)) (M.toList modelGraphPropImportance)
            ++ map (first ("Step:" <>)) (M.toList modelStepPropImportance)

    sortedFeatures :: [(Text, Double)]
    sortedFeatures = sortBy (flip (comparing snd)) features

    sortedUnknown :: [(Int, Set Int64)]
    sortedUnknown = sortBy (flip (comparing fst)) modelUnknownPreds

querySink
    :: (MonadResource m, MonadThrow m, Show a)
    => Maybe String
    -> FilePath
    -> ConduitT a Void m ()
querySink Nothing _ = void C.await
querySink (Just suffix) name =
    C.map showText
    .| C.map (`T.snoc` '\n')
    .| C.encode C.utf8
    .| C.sinkFile (name <> suffix)

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

        modelTrainConfig <- lift getConfig

        modelId <- lift $ fst <$> trainModel algoId platformId ModelDesc{..}

        liftIO $ print (fromSqlKey modelId)

    QueryModel{getModel} -> do
        modelId <- fst <$> getModel
        getModelStats modelId >>= reportModelStats

    Validate{getAlgoId,getPlatformId,getModel} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        validateModel algoId platformId model trainConfig

    Evaluate{getAlgorithm,getPlatformId,getModel,defaultImpl,evaluateConfig} -> do
        algo <- getAlgorithm
        platId <- getPlatformId
        (modelId, model) <- getModel
        trainConfig <- getModelTrainingConfig modelId
        evaluateModel algo platId defaultImpl evaluateConfig model trainConfig

    Compare{getAlgoId,getPlatformId,compareConfig} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId
        compareImplementations algoId platformId compareConfig

    Export{getAlgoId,getModel,cppFile} -> do
        algoId <- getAlgoId
        impls <- queryImplementations algoId
        (modelId, model) <- getModel
        TrainConfig{..} <- getModelTrainingConfig modelId
        dumpCppModel cppFile model trainGraphProps trainStepProps
            (implementationName <$> impls)

    QueryTest{getAlgoId,getPlatformId,outputSuffix} -> do
        algoId <- getAlgoId
        platformId <- getPlatformId

        graphPropQuery <- getDistinctFieldQuery GraphPropProperty
        stepPropQuery <- getDistinctFieldQuery StepPropProperty

        graphprops <- runSqlQueryConduit graphPropQuery $ C.foldMap S.singleton
        stepprops <- runSqlQueryConduit stepPropQuery $ C.foldMap S.singleton

        let stepQuery :: Query StepInfo
            stepQuery = stepInfoQuery algoId platformId graphprops stepprops

            variantQuery :: Query VariantInfo
            variantQuery = variantInfoQuery algoId platformId

        runSqlQueryConduit stepQuery $ querySink outputSuffix "stepInfoQuery-"
        runSqlQueryConduit variantQuery $ querySink outputSuffix "variantInfoQuery-"
