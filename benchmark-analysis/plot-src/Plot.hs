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
import Data.Functor.Compose (Compose(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Sqlite (Entity(..), (==.))
import qualified Database.Persist.Sqlite as Sql
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import System.IO (Handle, hClose, hPutStr, stdout)
import System.Process

import Core
import OptionParsers
import Paths_benchmark_analysis (getDataFileName)
import Query
import Schema

queryImplementations :: SqlM (IntMap Implementation)
queryImplementations = do
    Just (Entity aId _) <- selectBfs
    runConduitRes $ selectImpls aId .| C.foldMap toIntMap
  where
    selectBfs = Sql.selectFirst [ AlgorithmName ==. "bfs" ] []

    selectImpls aId = Sql.selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryVariants :: Set Text -> SqlM (Set (Key Variant))
queryVariants graphs = do
    Just (Entity aId _) <- selectBfs
    gids <- runConduitRes $ Sql.selectSource [] []
        .| C.filter (\i -> S.member (graphName (entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton
    variants <- forM (S.toList gids) $ \gId ->
        Sql.getBy $ UniqVariant gId aId "default"

    return . S.fromList . map Sql.entityKey . catMaybes $ variants
  where
    selectBfs = Sql.selectFirst [ AlgorithmName ==. "bfs" ] []

data PlotCommand
    = PlotLevels
      { printStdout :: Bool
      , getGpuId :: SqlM (Key GPU)
      , getGraphs :: SqlM (Set Text)
      , getImplementations :: SqlM (IntMap Implementation)
      }
    | PlotTotals
      { normalise :: Bool
      , printStdout :: Bool
      , getGpuId :: SqlM (Key GPU)
      , getGraphs :: SqlM (Set Text)
      , getImplementations :: SqlM (IntMap Implementation)
      }

type SqlParser = Compose Parser SqlM

commands :: String -> (InfoMod a, Parser PlotCommand)
commands name = (,) mempty . hsubparser $ mconcat
    [ subCommand "levels" "plot level times for a graph" "" $
        PlotLevels <$> printFlag <*> gpuParser <*> graphs <*> impls
    , subCommand "totals" "plot total times for set of graphs" "" $
        PlotTotals <$> normaliseFlag <*>  printFlag <*> gpuParser <*> graphs
                   <*> impls
    ]
  where
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting"]

    normaliseFlag = flag False True $ mconcat [long "normalise"]

    graphs = getCompose $ readSet "graphs"
    impls = getCompose $ filterImpls <$> readSet "implementations"
                                     <*> Compose (pure queryImplementations)

    readSet :: FilePath -> SqlParser (Set Text)
    readSet s = Compose . fmap readText . strOption $ mconcat
        [ metavar "FILE", long s
        , help $ "File to read " ++ s ++ " to plot from"
        ]

    readText :: MonadIO m => FilePath -> m (Set Text)
    readText = liftIO . fmap (S.fromList . T.lines) . T.readFile

    filterImpls :: Set Text -> IntMap Implementation -> IntMap Implementation
    filterImpls textSet = IM.filter $ (`S.member` textSet) . implementationName

reportData :: MonadIO m => Handle -> Bool -> (String, Vector Double) -> m ()
reportData hnd normalise (graph, timings) = liftIO $ do
    hPutStr hnd $ graph <> " :"
    VS.forM_ processedTimings $ \time -> hPutStr hnd $ " " ++ show time
    T.hPutStrLn hnd ""
  where
    processedTimings
        | normalise = VS.map (/ VS.maximum timings) timings
        | otherwise = timings

dataFromVariantInfo :: VariantInfo -> SqlM (String, Vector Double)
dataFromVariantInfo VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- graphName <$> Sql.getJust graphId
    return (T.unpack name, extendedTimings)
  where
    extendedTimings =
      variantTimings `VS.snoc` variantBestNonSwitching `VS.snoc` variantOptimal

plot
    :: String
    -> String
    -> IntMap Implementation
    -> Bool
    -> Bool
    -> Query a
    -> ConduitT a (String, Vector Double) SqlM ()
    -> SqlM ()
plot plotName axisName impls printStdout normalise query convert
    | printStdout = doWithHandle stdout
    | otherwise = do
        scriptProc <- liftIO $ proc <$> getDataFileName "scripts/bar-plot.py"
        let plotProc = (scriptProc [plotName, axisName, show normalise])
                { std_in = CreatePipe }

        withRunInIO $ \runInIO ->
            withCreateProcess plotProc . withProcess $ runInIO . doWithHandle
  where
    names = map (getImplName . snd) $ IM.toAscList impls
    indices = IS.fromList . map (subtract 1 . fst) . IM.toAscList $ impls
    filterTimes = VS.ifilter (const . (`IS.member` indices))

    withProcess work (Just plotHnd) Nothing Nothing procHandle = void $ do
        work plotHnd
        hClose plotHnd
        waitForProcess procHandle

    withProcess _ _ _ _ _ = throwM . Error $ "Error creating plot process!"

    doWithHandle hnd = do
        liftIO . T.hPutStrLn hnd $ T.intercalate ":" names
        runSqlQuery query $
            convert
            .| C.map (second filterTimes)
            .| C.mapM_ (reportData hnd normalise)

main :: IO ()
main = runSqlM commands $ \case
    PlotLevels{..} -> do
        impls <- getImplementations
        gpuId <- getGpuId
        variants <- getGraphs >>= queryVariants

        forM_ variants $ \variantId -> do
            graphId <- variantGraphId <$> Sql.getJust variantId
            name <- graphName <$> Sql.getJust graphId

            let query = levelTimePlotQuery gpuId variantId
                pdfName = T.unpack name ++ "-levels"

            plot pdfName "Levels" impls printStdout False query $
                C.map (first show)

    PlotTotals{..} -> do
        allImpls <- queryImplementations
        gpuId <- getGpuId
        keepVariants <- getGraphs >>= queryVariants

        let query = variantInfoQuery gpuId
            variantFilter VariantInfo{variantId} =
                S.member variantId keepVariants
            implCount = IM.size allImpls
            extImpls = IM.fromList
                [ (implCount + 1, mkImpl (toSqlKey 1) "Non-switching Best")
                , (implCount + 2, mkImpl (toSqlKey 1) "Optimal")
                ]
        impls <- getImplementations

        plot "times" "Graph" (impls <> extImpls) printStdout normalise query $
            C.filter variantFilter .| C.mapM dataFromVariantInfo
  where
    mkImpl gpuId name = Implementation gpuId name Nothing Nothing Core True
