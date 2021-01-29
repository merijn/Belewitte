{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PlotOptions (PlotCommand(..), commands) where

import Control.Monad (forM)
import Data.Bifunctor (bimap)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C
import Data.Foldable (asum)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Schema
import Sql ((==.))
import qualified Sql
import Options

import BarPlot
import GlobalPlotOptions
import Heatmap
import Interesting (ImplFilter)
import Query.Dump (plotQueryDump)
import Query.Level (levelTimePlotQuery)
import Query.Time (timePlotQuery)
import Query.Variant (VariantInfoConfig(..))

data PlotCommand
    = PlotBar BarPlot
    | PlotHeatmap Heatmap
    | ReportInteresting
        { optVariantConfig :: Maybe (Key VariantConfig)
        , variantConfigInfo :: VariantInfoConfig
        , implFilter :: ImplFilter
        , shortSummary :: Bool
        }

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- Sql.selectSource [] [] $
        C.filter (\i -> S.member (graphName (Sql.entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton

    variantConfigId <- Sql.selectKeysList
        [VariantConfigAlgorithmId ==. algoId, VariantConfigIsDefault ==. Active]
        [] >>= \case
            [key] -> return key
            [] -> logThrowM . PatternFailed $
              "No default variant config for algorithm #" <> showSqlKey algoId
            _ -> logThrowM . PatternFailed . mconcat $
                [ "Multiple default variant configs for algorithm #"
                , showSqlKey algoId ]

    variants <- forM (S.toList gids) $ \gId ->
        Sql.getBy $ UniqVariant gId variantConfigId

    return . S.fromList . map Sql.entityKey . catMaybes $ variants

readTextSet :: String -> Parser (SqlM (Set Text))
readTextSet name = readSet
  where
    readSet :: Parser (SqlM (Set Text))
    readSet = fmap readText . strOption $ mconcat
        [ metavar "FILE", long name
        , help $ "File to read " ++ name ++ " to plot from"
        ]

    readText :: MonadIO m => FilePath -> m (Set Text)
    readText = liftIO . fmap (S.fromList . T.lines) . T.readFile

filterImpls
    :: Set Text
    -> (IntMap Implementation, IntMap ExternalImpl)
    -> (IntMap Implementation, IntMap ExternalImpl)
filterImpls is = bimap (filterImplementation is) (filterExternal is)
  where
    filterImplementation
        :: Set Text -> IntMap Implementation -> IntMap Implementation
    filterImplementation textSet = IM.filter $ getAny . mconcat
        [ Any . (`S.member` textSet) . implementationName
        , Any . (Builtin==) . implementationType
        ]

    filterExternal :: Set Text -> IntMap ExternalImpl -> IntMap ExternalImpl
    filterExternal names = IM.filter ((`S.member` names) . externalImplName)

barPlotParser :: Parser (BarPlotType -> SqlM BarPlot)
barPlotParser = do
    getGlobalOpts <- globalOptionsParser
    slideFormat <- slideFlag
    printStdout <- printFlag
    getGraphs <- readTextSet "graphs"
    getImpls <- readTextSet "implementations"

    pure $ \barPlotType -> do
        rawImpls <- getImpls
        rawGraphs <- getGraphs

        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts

        let finalGlobalOpts = globalOpts
              { globalPlotImpls = filterImpls rawImpls globalPlotImpls }

        BarPlot finalGlobalOpts barPlotType slideFormat printStdout
            <$> queryVariants globalPlotAlgorithm rawGraphs
  where
    slideFlag :: Parser Bool
    slideFlag = flag False True $ mconcat
        [ long "slide", help "Render 4:3 slide dimensions" ]

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

variantSelectionOption :: Parser (Key Algorithm -> SqlM VariantSelection)
variantSelectionOption = asum
    [ fmap (fmap ConfigSelection) <$> variantConfigIdParser
    , pure $ const (return Everything)
    ]

totalsHeatmapParser :: Parser (SqlM Heatmap)
totalsHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantSelection <- variantSelectionOption
    showOptimal <- showOptimalFlag
    getDatasetId <- optional datasetIdParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        TotalHeatmap globalOpts
            <$> getVariantSelection globalPlotAlgorithm
            <*> sequence getDatasetId <*> pure showOptimal
  where
    showOptimalFlag :: Parser Bool
    showOptimalFlag = flag False True $ mconcat [long "show-optimal"]

levelsHeatmapParser :: Parser (SqlM Heatmap)
levelsHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantId <- variantIdParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        LevelHeatmap globalOpts <$> getVariantId globalPlotAlgorithm

predictHeatmapParser :: Parser (SqlM Heatmap)
predictHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantSelection <- variantSelectionOption
    getDatasetId <- optional datasetIdParser
    getPredictorConfigs <- predictorConfigsParser
    getImpls <- optional $ readTextSet "implementations"

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        keepImpls <- sequence getImpls

        let finalGlobalOpts = globalOpts
              { globalPlotImpls = case keepImpls of
                  Just impls -> filterImpls impls globalPlotImpls
                  Nothing -> globalPlotImpls
              }

        PredictHeatmap finalGlobalOpts
            <$> getVariantSelection globalPlotAlgorithm
            <*> sequence getDatasetId <*> getPredictorConfigs

commands :: CommandRoot (SqlM PlotCommand)
commands = CommandRoot
  { mainHeaderDesc = "a tool for plotting benchmark results"
  , mainDesc = ""
  , mainQueryDump = plotQueryDump
  , mainQueryMap = plotQueryMap
  , mainCommands = SubCommands
    [ fmap PlotBar <$> CommandGroup CommandInfo
        { commandName = "bar"
        , commandHeaderDesc = "bar plots"
        , commandDesc = ""
        }
        [ SingleCommand CommandInfo
            { commandName = "levels"
            , commandHeaderDesc = "plot level times for a graph"
            , commandDesc = ""
            }
            $ barPlotParser <*> pure Levels
        , SingleCommand CommandInfo
            { commandName = "totals"
            , commandHeaderDesc = "plot total times for a set of graphs"
            , commandDesc = ""
            }
            $ barPlotParser <*> (Totals <$> normaliseFlag)
        , SingleCommand CommandInfo
            { commandName = "vs-optimal"
            , commandHeaderDesc =
            "plot total times for a set of graphs against the optimal"
            , commandDesc = ""
            }
            $ barPlotParser <*> (VsOptimal <$> normaliseFlag)
        ]
    , fmap PlotHeatmap <$> CommandGroup CommandInfo
        { commandName = "heatmap"
        , commandHeaderDesc = "heatmap plots"
        , commandDesc = ""
        }
        [ SingleCommand CommandInfo
            { commandName = "total"
            , commandHeaderDesc = "plot heatmap for all variants"
            , commandDesc = ""
            }
            $ totalsHeatmapParser
        , SingleCommand CommandInfo
            { commandName = "levels"
            , commandHeaderDesc = "plot heatmap for a variant"
            , commandDesc = ""
            }
            $ levelsHeatmapParser
        , SingleCommand CommandInfo
            { commandName = "predict"
            , commandHeaderDesc = "plot heatmap for a predictor"
            , commandDesc = ""
            }
            $ predictHeatmapParser
        ]
    , SingleCommand CommandInfo
        { commandName = "report"
        , commandHeaderDesc = "report interesting variants"
        , commandDesc = "Highlights variants of interest for various criteria"
        }
        $ reportParser
    ]
  }
  where
    normaliseFlag :: Parser Bool
    normaliseFlag = flag False True $ mconcat [long "normalise"]

    implFilter :: Parser ImplFilter
    implFilter = implementations <|> pure id

    implementations :: Parser ImplFilter
    implementations = fmap mkFilter . option intervalReader $ mconcat
        [ metavar "ID", long "impl-set"
        , help "Range(s) of implementation ids to print results for. Accepts \
            \comma-seperated ranges. A range is dash-separated inclusive \
            \range or a single number. Example: \
            \--impl-set=5-10,13,17-20"
        ]
      where
        mkFilter :: IntervalSet Int -> ImplFilter
        mkFilter intervals = IM.filterWithKey (\k _ -> k `IS.member` intervals)

    summaryFlag :: Parser Bool
    summaryFlag = switch $ mconcat
        [ long "summary"
        , help "Print only a short summary of the interesting variants."
        ]

    reportParser :: Parser (SqlM PlotCommand)
    reportParser = do
        getVariantInfoConfig <- variantInfoConfigParser
        getVariantConfigId <- optional variantConfigIdParser
        filterFun <- implFilter
        summary <- summaryFlag

        pure $ do
            cfg <- getVariantInfoConfig
            variantConfigId <- sequence $
                getVariantConfigId <*> pure (variantInfoAlgorithm cfg)

            return $ ReportInteresting variantConfigId cfg filterFun summary

plotQueryMap :: Map String (Parser DebugQuery)
plotQueryMap = M.fromList
    [ nameDebugQuery "timePlotQuery" . Compose $ do
        getAlgorithmId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommit <- commitIdParser
        getVariants <- variantsParser

        pure $ do
            algoId <- getAlgorithmId
            timePlotQuery algoId
                <$> getPlatformId <*> getCommit algoId <*> getVariants algoId
    , nameDebugQuery "levelTimePlotQuery" . Compose $ do
        getAlgorithmId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommit <- commitIdParser
        getVariantId <- variantIdParser

        pure $ do
            algoId <- getAlgorithmId
            levelTimePlotQuery
                <$> getPlatformId <*> getCommit algoId <*> getVariantId algoId
    ]
  where
    variantsParser :: Parser (Key Algorithm -> SqlM (Set (Key Variant)))
    variantsParser = do
        variants <- some variantIdParser
        pure $ \algoId -> S.fromList <$> traverse ($algoId) variants
