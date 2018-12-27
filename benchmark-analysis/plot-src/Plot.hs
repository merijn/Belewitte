{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Unlift (withRunInIO)
import Data.Bifunctor (first, second)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Entity(..), (==.))
import qualified Database.Persist.Sqlite as Sql
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import System.IO (Handle, hClose, hPutStr, stdout)
import System.Process

import Core
import OptionParsers
import Paths_benchmark_analysis (getDataFileName)
import Query
import Schema

queryImplementations :: Key Algorithm -> SqlM (IntMap Implementation)
queryImplementations algoId = runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = Sql.selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- runConduitRes $ Sql.selectSource [] []
        .| C.filter (\i -> S.member (graphName (entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton
    variants <- forM (S.toList gids) $ \gId ->
        Sql.getBy $ UniqVariant gId algoId "default"

    return . S.fromList . map Sql.entityKey . catMaybes $ variants

data PlotType = PlotLevels | PlotTotals

data PlotConfig = PlotConfig
    { axisName :: String
    , normalise :: Bool
    , printStdout :: Bool
    }

data PlotOptions =
    PlotOptions
      { plotType :: PlotType
      , getAlgoId :: SqlM (Key Algorithm)
      , getGpuId :: SqlM (Key GPU)
      , getGraphs :: SqlM (Set Text)
      , getImplementations :: Key Algorithm -> SqlM (IntMap Implementation)
      , plotConfig :: PlotConfig
      }

plotOptions :: PlotType -> Parser PlotOptions
plotOptions plottype =
  PlotOptions plottype <$> algorithmParser <*> gpuParser <*> graphs <*> impls
                       <*> config
  where
    baseConfig = case plottype of
        PlotLevels -> pure $ PlotConfig "Levels" False
        PlotTotals -> PlotConfig "Graph" <$> normaliseFlag

    config :: Parser PlotConfig
    config = baseConfig <*> printFlag

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

    normaliseFlag :: Parser Bool
    normaliseFlag = flag False True $ mconcat [long "normalise"]

    graphs :: Parser (SqlM (Set Text))
    graphs = readSet "graphs"

    impls :: Parser (Key Algorithm -> SqlM (IntMap Implementation))
    impls = do
        keepImpls <- readSet "implementations"
        return $ \algoId ->
            filterImpls <$> keepImpls <*> queryImplementations algoId

    readSet :: FilePath -> Parser (SqlM (Set Text))
    readSet s = fmap readText . strOption $ mconcat
        [ metavar "FILE", long s
        , help $ "File to read " ++ s ++ " to plot from"
        ]

    readText :: MonadIO m => FilePath -> m (Set Text)
    readText = liftIO . fmap (S.fromList . T.lines) . T.readFile

    filterImpls :: Set Text -> IntMap Implementation -> IntMap Implementation
    filterImpls textSet = IM.filter $ (`S.member` textSet) . implementationName


commands :: String -> (InfoMod a, Parser PlotOptions)
commands name = (,) mempty . hsubparser $ mconcat
    [ subCommand "levels" "plot level times for a graph" "" $
        plotOptions PlotLevels
    , subCommand "totals" "plot total times for set of graphs" "" $
        plotOptions PlotTotals
    ]
  where
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

reportData :: MonadIO m => Handle -> Bool -> (Text, Vector (Int64, Double)) -> m ()
reportData hnd normalise (graph, timingsPair) = liftIO $ do
    T.hPutStr hnd $ graph <> " :"
    VU.forM_ processedTimings $ \time -> hPutStr hnd $ " " ++ show time
    T.hPutStrLn hnd ""
  where
    timings :: Vector Double
    timings = VU.map snd timingsPair

    processedTimings :: Vector Double
    processedTimings
        | normalise = VU.map (/ VU.maximum timings) timings
        | otherwise = timings

dataFromVariantInfo :: VariantInfo -> SqlM (Text, Vector (Int64,Double))
dataFromVariantInfo VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- graphName <$> Sql.getJust graphId
    return (name, extendedTimings)
  where
    extendedTimings =
      variantTimings `VU.snoc` (100, variantBestNonSwitching) `VU.snoc` (200, variantOptimal)

plot
    :: PlotConfig
    -> Text
    -> IntMap Implementation
    -> Query a
    -> ConduitT a (Text, Vector (Int64, Double)) SqlM ()
    -> SqlM ()
plot PlotConfig{..} plotName impls query convert
  | printStdout = doWithHandle stdout
  | otherwise = do
        scriptProc <- liftIO $
            proc <$> getDataFileName "runtime-data/scripts/bar-plot.py"

        let plotProc = (scriptProc [T.unpack plotName, axisName, show normalise])
                { std_in = CreatePipe }

        withRunInIO $ \runInIO ->
            withCreateProcess plotProc . withProcess $ runInIO . doWithHandle
  where
    isRelevant (i, _) = IM.member (fromIntegral i) impls

    names = map (getImplName . snd) $ IM.toAscList impls

    withProcess work (Just plotHnd) Nothing Nothing procHandle = void $ do
        work plotHnd
        hClose plotHnd
        waitForProcess procHandle

    withProcess _ _ _ _ _ = throwM . Error $ "Error creating plot process!"

    doWithHandle hnd = do
        liftIO . T.hPutStrLn hnd $ T.intercalate ":" names
        runSqlQuery query $
            convert
            .| C.map (second (VU.filter isRelevant))
            .| C.mapM_ (reportData hnd normalise)

main :: IO ()
main = runSqlM commands $ \PlotOptions{..} -> do
    algoId <- getAlgoId
    gpuId <- getGpuId
    impls <- getImplementations algoId
    variants <- getGraphs >>= queryVariants algoId

    case plotType of
        PlotLevels -> do
            forM_ variants $ \variantId -> do
                graphId <- variantGraphId <$> Sql.getJust variantId
                name <- graphName <$> Sql.getJust graphId

                let query = levelTimePlotQuery gpuId variantId
                    pdfName = name <> "-levels"

                plot plotConfig pdfName impls query $
                    C.map (first showText)

        PlotTotals -> do
            let variantQuery = variantInfoQuery algoId gpuId
                timeQuery = timePlotQuery algoId gpuId variants
                variantFilter VariantInfo{variantId} =
                    S.member variantId variants
                extImpls = IM.fromList
                    [ (100, mkImpl (toSqlKey 1) "Non-switching Best")
                    , (200, mkImpl (toSqlKey 1) "Optimal")
                    ]
                plotImpls = impls <> extImpls

            plot plotConfig "times-variant" plotImpls variantQuery $
                C.filter variantFilter .| C.mapM dataFromVariantInfo

            plot plotConfig "times-timeplot" impls timeQuery $
                awaitForever yield
  where
    mkImpl gpuId name = Implementation gpuId name Nothing Nothing Core True
