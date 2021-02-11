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

import Control.Monad (forM_)
import Data.Binary.Put (putDoublehost, runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (hClose)

import ProcessTools (Inherited(..), ReadWrite(Write))
import qualified ProcessTools as Proc

import Core
import Predictor
    ( PredictorConfig
    , loadPredictor
    , rawPredictorId
    , toPredictorName
    )
import Query
import Query.Step
import Query.StepHeatmap
import Query.Variant
import RuntimeData (getHeatmapScript)
import Schema
import StepAggregate (VariantAggregate(..), stepAggregator)
import Utils.ImplTiming
import Utils.Pair (Pair(..))

import GlobalPlotOptions

data VariantSelection
    = ConfigSelection (Key VariantConfig)
    | Everything

data Heatmap
    = TotalHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariantSelection :: VariantSelection
      , heatmapDatasets :: Maybe (Set (Key Dataset))
      , heatmapShowOptimal :: Bool
      }
    | LevelHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariant :: Key Variant
      }
    | PredictHeatmap
      { heatmapGlobalOpts :: GlobalPlotOptions
      , heatmapVariantSelection :: VariantSelection
      , heatmapDatasets :: Maybe (Set (Key Dataset))
      , heatmapPredictors :: [PredictorConfig]
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
        , variantInfoVariantConfigs = case heatmapVariantSelection of
            Everything -> Nothing
            ConfigSelection n -> Just (S.singleton n)
        , variantInfoDatasets = heatmapDatasets
        , variantInfoTimestamp = globalPlotTimestamp
        , variantInfoAllowNewer = globalPlotAllowNewer
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

    predictors <- mapM loadPredictor heatmapPredictors
    predictorNames <- mapM (toPredictorName . rawPredictorId) predictors

    let stepCfg = StepInfoConfig
            { stepInfoAlgorithm = globalPlotAlgorithm
            , stepInfoPlatform = globalPlotPlatform
            , stepInfoCommit = globalPlotCommit
            , stepInfoFilterIncomplete = True
            , stepInfoAllowNewer = NoNewer
            , ..
            }

        aggregateQuery = variantConduit
            .> streamQuery . stepInfoQuery stepCfg
            .| stepAggregator predictors
            .| C.map normalise

    numSteps <- runRegionConduit $ aggregateQuery .| C.length

    runPlotScript (regular implNames <> IM.fromList predictorNames) numSteps $
        aggregateQuery .| C.map (regular . implTimes)
  where
    implNames = toImplNames id id globalPlotImpls

    keepImpl :: IntMap v -> ImplTiming -> Bool
    keepImpl implMap ImplTiming{implTimingImpl} =
      implTimingImpl < 0 || IM.member (fromIntegral implTimingImpl) implMap

    filterFuns :: Pair (VS.Vector ImplTiming -> VS.Vector ImplTiming)
    filterFuns = VS.filter . keepImpl <$> implNames

    normalise :: VariantAggregate -> VariantAggregate
    normalise agg@VariantAgg{optimalTime, implTimes} = agg
      { implTimes = VS.map normaliseTiming <$> (filterFuns <*> implTimes) }
      where
        normaliseTiming :: ImplTiming -> ImplTiming
        normaliseTiming (ImplTiming implId timing) =
            ImplTiming implId (timing / optimalTime)

    variantConduit = case heatmapDatasets of
        Nothing -> streamQuery (algorithmVariantQuery globalPlotAlgorithm)
        Just d -> forM_ d C.yield .>
            streamQuery . datasetVariantQuery globalPlotAlgorithm

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
                .| C.map putTimings
                .| C.sinkHandle dataHnd

            liftIO $ hClose dataHnd
  where
    lookupName :: ImplTiming -> Text
    lookupName ImplTiming{implTimingImpl} =
        IM.findWithDefault "?" (fromIntegral implTimingImpl) implNames

putTimings :: VS.Vector ImplTiming -> ByteString
putTimings = LBS.toStrict . runPut . VS.mapM_ (putDoublehost . implTimingTiming)
