{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad (forM, forM_)
import Control.Monad.Catch (handle)
import Data.Bifunctor (bimap, second)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (chunksOf)
import qualified Data.Conduit.Text as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Storable as Storable
import System.IO (Handle, hPutStr, stdout)

import Core
import Options
import PlotQuery (timePlotQuery, levelTimePlotQuery)
import Utils.Process (withStdin)
import Query
import RuntimeData (getBarPlotScript)
import Schema
import Sql (SelectOpt(Asc), (==.))
import qualified Sql
import Utils.ImplTiming
import Utils.Pair (Pair(..), toPair, mergePair)
import VariantQuery

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
      , algoId :: Key Algorithm
      , platformId :: Key Platform
      , commitId :: CommitId
      , graphSet :: Set Text
      , implementationNames :: Set Text
      }

plotOptions :: PlotType -> Parser (PlotConfig -> SqlM PlotOptions)
plotOptions pType = do
    getAlgoId <- algorithmIdParser
    getPlatformId <- platformIdParser
    getCommitId <- commitIdParser
    getGraphs <- graphs
    getImpls <- impls

    pure $ \plotConfig -> do
        algoId <- getAlgoId
        PlotOptions pType plotConfig algoId
            <$> getPlatformId <*> getCommitId algoId <*> getGraphs <*> getImpls
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

filterImpls :: Set Text -> IntMap Implementation -> IntMap Implementation
filterImpls textSet = IM.filter $ getAny . mconcat
    [ Any . (`S.member` textSet) . implementationName
    , Any . (Builtin==) . implementationType
    ]

filterExternal :: Set Text -> IntMap ExternalImpl -> IntMap ExternalImpl
filterExternal names = IM.filter ((`S.member` names) . externalImplName)

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
        $ plotOptions PlotLevels <*> (plotConfig "Levels" <*> pure False)
    , SingleCommand CommandInfo
        { commandName = "totals"
        , commandHeaderDesc = "plot total times for a set of graphs"
        , commandDesc = ""
        }
        $ plotOptions PlotTotals <*> (plotConfig "Graph" <*> normaliseFlag)
    , SingleCommand CommandInfo
        { commandName = "vs-optimal"
        , commandHeaderDesc =
          "plot total times for a set of graphs against the optimal"
        , commandDesc = ""
        }
        $ plotOptions PlotVsOptimal <*> (plotConfig "Graph" <*> normaliseFlag)
    ]
  }
  where
    plotConfig :: String -> Parser (Bool -> PlotConfig)
    plotConfig axis = PlotConfig axis <$> slideFlag <*> printFlag

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

reportData
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Handle -> Bool -> ConduitT (Text, Vector (Text, Double)) Void m ()
reportData hnd normalise = do
    impls <- C.peek >>= \case
        Just (_, v) -> return $ V.map fst v
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one output row"

    liftIO . T.hPutStrLn hnd $ toColumnLabels impls
    C.mapM_ $ printGraph impls
  where
    toColumnLabels :: Vector Text -> Text
    toColumnLabels = mconcat . intersperse ":" . V.toList

    printGraph
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Vector Text -> (Text, Vector (Text, Double)) -> m ()
    printGraph impls (graph, timingsPair)
        | mismatch = logThrowM $
            QueryResultMismatch (V.toList impls) (V.toList timingImpls)

        | otherwise = liftIO $ do
            T.hPutStr hnd $ graph <> " :"
            V.forM_ processedTimings $ \time -> hPutStr hnd $ " " ++ show time
            T.hPutStrLn hnd ""
      where
        mismatch :: Bool
        mismatch = not . V.and $ V.zipWith (==) impls timingImpls

        timingImpls :: Vector Text
        timings :: Vector Double
        (timingImpls, timings) = V.unzip timingsPair

        maxTime :: Double
        maxTime = V.maximum . V.filter (not . isInfinite) $ timings

        processedTimings :: Vector Double
        processedTimings
            | normalise = V.map (/ maxTime) timings
            | otherwise = timings

dataFromVariantInfo :: VariantInfo -> SqlM (Text, Pair (Vector ImplTiming))
dataFromVariantInfo VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- graphName <$> Sql.getJust graphId
    return (name, Pair extendedTimings (V.convert variantExternalTimings))
  where
    extendedTimings = V.convert variantTimings
        `V.snoc` ImplTiming bestNonSwitchingImplId variantBestNonSwitching
        `V.snoc` ImplTiming optimalImplId variantOptimal

plot
    :: PlotConfig
    -> Text
    -> Query a
    -> ConduitT a (Text, Vector (Text, Double)) SqlM ()
    -> SqlM ()
plot PlotConfig{..} plotName query convert
  | printStdout = doWithHandle stdout
  | otherwise = do
        plotProcess <- getBarPlotScript args
        withStdin plotProcess doWithHandle
  where
    args :: [String]
    args = [T.unpack plotName, axisName, show normalise, show slideFormat]

    doWithHandle :: Handle -> SqlM ()
    doWithHandle hnd = runSqlQueryConduit query $
        convert .| reportData hnd normalise

nameImplementations
    :: Generic.Vector v ImplTiming
    => IntMap Text
    -> v ImplTiming
    -> Vector (Text, Double)
nameImplementations impls = V.mapMaybe translate . V.convert
  where
    translate :: ImplTiming -> Maybe (Text, Double)
    translate (ImplTiming impl val) = (,val) <$> impls IM.!? fromIntegral impl

main :: IO ()
main = runSqlM commands $ \getPlotOptions -> do
    PlotOptions{..} <- getPlotOptions
    variants <- queryVariants algoId graphSet

    impls <- (,) <$> Sql.queryImplementations algoId
                 <*> Sql.queryExternalImplementations algoId

    let filteredImpls = filterImpls implementationNames
        filteredExt = filterExternal implementationNames
        implMaps@Pair{regular} = toImplNames filteredImpls filteredExt impls

        translatePair :: Pair (Vector ImplTiming) -> Vector (Text, Double)
        translatePair x = mergePair $ nameImplementations <$> implMaps <*> x

    case plotType of
        PlotLevels -> do
            forM_ variants $ \variantId -> do
                graphId <- variantGraphId <$> Sql.getJust variantId
                name <- graphName <$> Sql.getJust graphId

                let query = levelTimePlotQuery platformId commitId variantId
                    pdfName = name <> "-levels"
                    missingResults (PatternFailed _) = logErrorN $ mconcat
                        [ "Missing results for graph \"", name
                        , "\", variant #", showSqlKey variantId ]

                handle missingResults $ plot plotConfig pdfName query $
                    C.map (bimap showText (nameImplementations regular))

        PlotTotals -> do
            let timeQuery = timePlotQuery algoId platformId commitId variants

            plot plotConfig "times-totals" timeQuery $
                C.map (second $ translatePair . toPair V.convert V.convert)

        PlotVsOptimal -> do
            let variantQuery = variantInfoQuery $
                    VariantInfoConfig algoId platformId commitId Nothing False

                variantFilter VariantInfo{variantId} =
                    S.member variantId variants

            plot plotConfig "times-vs-optimal" variantQuery $
                C.filter variantFilter
                .| C.mapM dataFromVariantInfo
                .| C.map (second $ translatePair . fmap V.convert)

getConfigSet :: SqlM (Set (Key Algorithm, Key Platform, CommitId))
getConfigSet = Sql.selectSource [] [] $ C.foldMap (S.singleton . toTuple)
  where
    toTuple :: Entity RunConfig -> (Key Algorithm, Key Platform, CommitId)
    toTuple (Entity _ RunConfig{..}) =
        (runConfigAlgorithmId, runConfigPlatformId, runConfigAlgorithmVersion)

toTimeQueries
    :: (Key Algorithm, Key Platform, CommitId)
    -> ConduitT (Key Algorithm, Key Platform, CommitId)
                (Query (Text, ( Storable.Vector ImplTiming
                              , Storable.Vector ImplTiming
                              )
                       )
                )
                (Region SqlM)
                ()
toTimeQueries (algoId, platformId, commitId) =
  Sql.selectKeysRegion [VariantAlgorithmId ==. algoId] [Asc VariantId]
  .| C.chunksOf 500
  .| C.map (timePlotQuery algoId platformId commitId . S.fromList)

toLevelTimeQueries
    :: (Key Platform, CommitId)
    -> ConduitT (Key Platform, CommitId)
                (Query (Int64, Storable.Vector ImplTiming))
                (Region SqlM)
                ()
toLevelTimeQueries (platformId, commitId) =
  Sql.selectKeysRegion [] [Asc VariantId]
  .| C.map (levelTimePlotQuery platformId commitId)

plotQueryDump :: FilePath -> SqlM ()
plotQueryDump outputSuffix = do
    configSet <- getConfigSet

    Sql.runRegionConduit $
        C.yieldMany configSet
        .> toTimeQueries
        .> streamQuery
        .| querySink "timeQuery-"

    Sql.runRegionConduit $
        C.yieldMany (S.map stripAlgorithm configSet)
        .> toLevelTimeQueries
        .> streamQuery
        .| querySink "levelTimeQuery-"
  where
    stripAlgorithm
        :: (Key Algorithm, Key Platform, CommitId) -> (Key Platform, CommitId)
    stripAlgorithm (_, platformId, commitId) = (platformId, commitId)

    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => FilePath -> ConduitT a Void m ()
    querySink  name =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile (name <> outputSuffix)
