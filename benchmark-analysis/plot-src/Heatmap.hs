{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Heatmap
    ( VariantSelection(..)
    , Heatmap(..)
    , plotHeatmap
    ) where

import Control.Monad (forM)
import Data.Binary.Put (putDoublehost, runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (hClose)

import Core
import Predictor
    ( MispredictionStrategy(None)
    , loadPredictor
    , predictorModelId
    , toPredictorName
    )
import Query
import RuntimeData (getHeatmapScript)
import Schema
import StepAggregate (VariantAggregate(..), stepAggregator)
import StepQuery
import StepHeatmapQuery
import Utils.ImplTiming
import Utils.Pair (Pair(..))
import Utils.Process (Inherited(..), ReadWrite(Write))
import qualified Utils.Process as Proc
import VariantQuery

import GlobalPlotOptions

data VariantSelection = ConfigSelection (Key VariantConfig) | Everything

data Heatmap
    = TotalHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariantSelection :: VariantSelection
      , heatmapDataset :: Maybe (Key Dataset)
      , heatmapShowOptimal :: Bool
      }
    | LevelHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariant :: Key Variant
      }
    | PredictHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariantSelection :: VariantSelection
      , heatmapDataset :: Maybe (Key Dataset)
      , heatmapPredictors :: [Key PredictionModel]
      }

plotHeatmap :: Heatmap -> SqlM ()
plotHeatmap TotalHeatmap{heatmapGlobalOpts = GlobalPlotOptions{..}, ..} = do
    numVariants <- runSqlQueryCount variantQuery
    runPlotScript implNames numVariants $ streamQuery variantQuery
  where
    Pair implNames _ = toImplNames id id globalPlotImpls

    extractTimings :: VariantInfo -> Vector ImplTiming
    extractTimings VariantInfo{..}
        | heatmapShowOptimal = extendTimings variantTimings
        | otherwise = variantTimings
      where
        extendTimings = VS.cons (ImplTiming optimalImplId variantOptimal)

    variantQuery = extractTimings <$> variantInfoQuery VariantInfoConfig
        { variantInfoAlgorithm = globalPlotAlgorithm
        , variantInfoPlatform = globalPlotPlatform
        , variantInfoCommit = globalPlotCommit
        , variantInfoVariantConfig = case heatmapVariantSelection of
            Everything -> Nothing
            ConfigSelection n -> Just n
        , variantInfoDataset = heatmapDataset
        , variantInfoFilterIncomplete = False
        }

plotHeatmap LevelHeatmap{heatmapGlobalOpts = GlobalPlotOptions{..}, ..} = do
    numSteps <- runSqlQueryCount stepQuery
    runPlotScript implNames numSteps $ streamQuery stepQuery
  where
    Pair implNames _ = toImplNames id id globalPlotImpls

    stepQuery = stepHeatmapTimings <$> stepHeatmapQuery StepHeatmapConfig
        { stepHeatmapAlgorithm = globalPlotAlgorithm
        , stepHeatmapPlatform = globalPlotPlatform
        , stepHeatmapCommit = globalPlotCommit
        , stepHeatmapVariant = heatmapVariant
        }

plotHeatmap PredictHeatmap{heatmapGlobalOpts = GlobalPlotOptions{..}, ..} = do
    stepInfoTimestamp <- liftIO getCurrentTime

    predictors <- forM heatmapPredictors $ \p -> loadPredictor p None
    predictorNames <- mapM (toPredictorName . predictorModelId) predictors

    let stepCfg = StepInfoConfig
            { stepInfoAlgorithm = globalPlotAlgorithm
            , stepInfoPlatform = globalPlotPlatform
            , stepInfoCommit = globalPlotCommit
            , stepInfoFilterIncomplete = True
            , ..
            }

        aggregateQuery = variantConduit
            .> streamQuery . stepInfoQuery stepCfg
            .| stepAggregator predictors

    numSteps <- runRegionConduit $ aggregateQuery .| C.length

    runPlotScript (implNames <> IM.fromList predictorNames) numSteps $
        aggregateQuery .| C.map (regular . implTimes)
  where
    implNames = regular $ toImplNames id id globalPlotImpls

    variantConduit = case heatmapDataset of
        Nothing -> streamQuery (algorithmVariantQuery globalPlotAlgorithm)
        Just d -> streamQuery (datasetVariantQuery globalPlotAlgorithm d)

runPlotScript
    :: IntMap Text
    -> Int
    -> ConduitT () (Vector ImplTiming) (Region SqlM) ()
    -> SqlM ()
runPlotScript implNames rowCount queryDataConduit = do
    (implCount, implVec) <- runRegionConduit $ queryDataConduit .| do
        firstVec <- C.head
        case firstVec of
            Just v -> return (VS.length v, v)
            Nothing -> logThrowM . PatternFailed $
                "Unable to compute implementation count for results"

    Proc.runProcessCreation_ $ do
        (dataFd, dataHnd) <- Proc.withPipe Write
        plotProcess <- lift $
            getHeatmapScript [dataFd, show rowCount, show implCount, "out"]

        Proc.withProcess plotProcess $ \columnHnd Inherited -> do
            VS.forM_ implVec $ liftIO . T.hPutStrLn columnHnd . lookupName

            liftIO $ hClose columnHnd

            runRegionConduit $
                queryDataConduit
                .| C.map (putTimings . normaliseVector)
                .| C.sinkHandle dataHnd

            liftIO $ hClose dataHnd
  where
    lookupName :: ImplTiming -> Text
    lookupName ImplTiming{implTimingImpl} =
        IM.findWithDefault "?" (fromIntegral implTimingImpl) implNames

normaliseVector :: VS.Vector ImplTiming -> VS.Vector Double
normaliseVector vec = VS.map (/ VS.maximum newVec) newVec
  where
    newVec = VS.map implTimingTiming vec

putTimings :: VS.Vector Double -> ByteString
putTimings = LBS.toStrict . runPut . VS.mapM_ putDoublehost
