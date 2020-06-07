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
import LevelQuery (levelTimePlotQuery)
import QueryDump (plotQueryDump)
import TimeQuery (timePlotQuery)

data PlotCommand = PlotBar BarPlot | PlotHeatmap Heatmap

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- Sql.selectSource [] [] $
        C.filter (\i -> S.member (graphName (Sql.entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton

    variantConfigId <- Sql.selectKeysList
        [VariantConfigAlgorithmId ==. algoId, VariantConfigIsDefault ==. True]
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

barPlotParser :: Parser (BarPlotType -> SqlM BarPlot)
barPlotParser = do
    getGlobalOpts <- globalOptionsParser
    slideFormat <- slideFlag
    printStdout <- printFlag
    getGraphs <- graphs
    getImpls <- impls

    pure $ \barPlotType -> do
        rawImpls <- getImpls
        rawGraphs <- getGraphs

        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts

        let finalGlobalOpts = globalOpts
              { globalPlotImpls = filterImpls rawImpls globalPlotImpls }

        BarPlot finalGlobalOpts barPlotType slideFormat printStdout
            <$> queryVariants globalPlotAlgorithm rawGraphs
  where
    graphs :: Parser (SqlM (Set Text))
    graphs = readSet "graphs"

    impls :: Parser (SqlM (Set Text))
    impls = readSet "implementations"

    readSet :: FilePath -> Parser (SqlM (Set Text))
    readSet s = fmap readText . strOption $ mconcat
        [ metavar "FILE", long s
        , help $ "File to read " ++ s ++ " to plot from"
        ]

    readText :: MonadIO m => FilePath -> m (Set Text)
    readText = liftIO . fmap (S.fromList . T.lines) . T.readFile

    slideFlag :: Parser Bool
    slideFlag = flag False True $ mconcat
        [ long "slide", help "Render 4:3 slide dimensions" ]

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

    filterImpls
        :: Set Text
        -> (IntMap Implementation, IntMap ExternalImpl)
        -> (IntMap Implementation, IntMap ExternalImpl)
    filterImpls is = bimap (filterImplementation is) (filterExternal is)

    filterImplementation
        :: Set Text -> IntMap Implementation -> IntMap Implementation
    filterImplementation textSet = IM.filter $ getAny . mconcat
        [ Any . (`S.member` textSet) . implementationName
        , Any . (Builtin==) . implementationType
        ]

    filterExternal :: Set Text -> IntMap ExternalImpl -> IntMap ExternalImpl
    filterExternal names = IM.filter ((`S.member` names) . externalImplName)

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
        globalOpts <- getGlobalOpts
        LevelHeatmap globalOpts <$> getVariantId

predictHeatmapParser :: Parser (SqlM Heatmap)
predictHeatmapParser = do
    getGlobalOpts <- globalOptionsParser
    getVariantSelection <- variantSelectionOption
    getDatasetId <- optional datasetIdParser
    getPredictorConfigs <- some predictorParser

    pure $ do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        PredictHeatmap globalOpts
            <$> getVariantSelection globalPlotAlgorithm
            <*> sequence getDatasetId <*> sequence getPredictorConfigs

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
    ]
  }
  where
    normaliseFlag :: Parser Bool
    normaliseFlag = flag False True $ mconcat [long "normalise"]

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
                <$> getPlatformId <*> getCommit algoId <*> getVariants
    , nameDebugQuery "levelTimePlotQuery" . Compose $ do
        getAlgorithmId <- algorithmIdParser
        getPlatformId <- platformIdParser
        getCommit <- commitIdParser
        getVariantId <- variantIdParser

        pure $ do
            algoId <- getAlgorithmId
            levelTimePlotQuery
                <$> getPlatformId <*> getCommit algoId <*> getVariantId
    ]
  where
    variantsParser :: Parser (SqlM (Set (Key Variant)))
    variantsParser = fmap S.fromList . sequence <$> some variantIdParser
