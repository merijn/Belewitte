{-# LANGUAGE ApplicativeDo #-}
module PlotOptions
    ( PlotConfig(..)
    , PlotOptions(..)
    , PlotType(..)
    , commands
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import LevelQuery (levelTimePlotQuery)
import Schema
import Options
import QueryDump (plotQueryDump)
import TimeQuery (timePlotQuery)

data PlotConfig = PlotConfig
    { axisName :: String
    , slideFormat :: Bool
    , printStdout :: Bool
    , normalise :: Bool
    }

data PlotType
    = PlotLevels
    | PlotTotals
    | PlotVsOptimal

data PlotOptions
    = PlotOptions
      { plotType :: PlotType
      , plotConfig :: PlotConfig
      , algorithmId :: Key Algorithm
      , platformId :: Key Platform
      , commitId :: CommitId
      , graphSet :: Set Text
      , implementationNames :: Set Text
      }

plotOptions :: PlotType -> Parser (PlotConfig -> SqlM PlotOptions)
plotOptions pType = do
    getAlgoId <- algorithmIdParser
    getPlatformId <- platformIdParser
    getCommit <- commitIdParser
    getGraphs <- graphs
    getImpls <- impls

    pure $ \config -> do
        algoId <- getAlgoId
        PlotOptions pType config algoId
            <$> getPlatformId <*> getCommit algoId <*> getGraphs <*> getImpls
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

commands :: CommandRoot (SqlM PlotOptions)
commands = CommandRoot
  { mainHeaderDesc = "a tool for plotting benchmark results"
  , mainDesc = ""
  , mainQueryDump = plotQueryDump
  , mainQueryMap = plotQueryMap
  , mainCommands =
    [ SingleCommand CommandInfo
        { commandName = "levels"
        , commandHeaderDesc = "plot level times for a graph"
        , commandDesc = ""
        }
        $ plotOptions PlotLevels <*> (plotConfigParser "Levels" <*> pure False)
    , SingleCommand CommandInfo
        { commandName = "totals"
        , commandHeaderDesc = "plot total times for a set of graphs"
        , commandDesc = ""
        }
        $ plotOptions PlotTotals <*>
            (plotConfigParser "Graph" <*> normaliseFlag)
    , SingleCommand CommandInfo
        { commandName = "vs-optimal"
        , commandHeaderDesc =
          "plot total times for a set of graphs against the optimal"
        , commandDesc = ""
        }
        $ plotOptions PlotVsOptimal <*>
                (plotConfigParser "Graph" <*> normaliseFlag)
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
