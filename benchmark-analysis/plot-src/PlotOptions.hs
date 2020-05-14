{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module PlotOptions
    ( BarPlot(..)
    , commands
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import Schema
import Options

import BarPlot
import GlobalPlotOptions
import LevelQuery (levelTimePlotQuery)
import QueryDump (plotQueryDump)
import TimeQuery (timePlotQuery)

barPlotParser :: BarPlotType -> Parser (PlotConfig -> SqlM BarPlot)
barPlotParser barPlotType = do
    getGlobalOpts <- globalOptionsParser
    getGraphs <- graphs
    getImpls <- impls

    pure $ \config -> do
        globalOpts@GlobalPlotOptions{..} <- getGlobalOpts
        BarPlot globalOpts config barPlotType <$> getGraphs <*> getImpls
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

commands :: CommandRoot (SqlM BarPlot)
commands = CommandRoot
  { mainHeaderDesc = "a tool for plotting benchmark results"
  , mainDesc = ""
  , mainQueryDump = plotQueryDump
  , mainQueryMap = plotQueryMap
  , mainCommands = SubCommands
    [ SingleCommand CommandInfo
        { commandName = "levels"
        , commandHeaderDesc = "plot level times for a graph"
        , commandDesc = ""
        }
        $ barPlotParser Levels <*> (plotConfigParser "Levels" <*> pure False)
    , SingleCommand CommandInfo
        { commandName = "totals"
        , commandHeaderDesc = "plot total times for a set of graphs"
        , commandDesc = ""
        }
        $ barPlotParser Totals <*> (plotConfigParser "Graph" <*> normaliseFlag)
    , SingleCommand CommandInfo
        { commandName = "vs-optimal"
        , commandHeaderDesc =
          "plot total times for a set of graphs against the optimal"
        , commandDesc = ""
        }
        $ barPlotParser VsOptimal <*> (plotConfigParser "Graph" <*> normaliseFlag)
    ]
  }
  where
    plotConfigParser :: String -> Parser (Bool -> PlotConfig)
    plotConfigParser axis = PlotConfig axis <$> slideFlag <*> printFlag

    slideFlag :: Parser Bool
    slideFlag = flag False True $ mconcat
        [ long "slide", help "Render 4:3 slide dimensions" ]

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

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
