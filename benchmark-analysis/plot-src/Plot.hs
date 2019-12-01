{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad (forM, forM_, void)
import Data.Bifunctor (bimap, second)
import Data.Char (isSpace)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..), (<>))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Generic
import System.IO (Handle, IOMode(WriteMode), hPutStr, stdout, withFile)

import Commands
import Core
import OptionParsers
import PlotQuery (timePlotQuery, levelTimePlotQuery)
import Utils.Process (withStdin)
import Query
import RuntimeData (getBarPlotScript)
import Schema
import Sql ((==.))
import qualified Sql
import Utils.ImplTiming
import Utils.Pair (Pair(..), toPair, mergePair)

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- runConduit $ Sql.selectSource [] []
        .| C.filter (\i -> S.member (graphName (Sql.entityVal i)) graphs)
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

data PlotType = PlotLevels | PlotTotals | PlotVsOptimal

data PlotConfig = PlotConfig
    { axisName :: String
    , normalise :: Bool
    , slideFormat :: Bool
    , printStdout :: Bool
    }

data PlotOptions
    = PlotOptions
      { plotType :: PlotType
      , getAlgoId :: SqlM (Key Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , getGraphs :: SqlM (Set Text)
      , getImplementations :: SqlM (Set Text)
      , plotConfig :: PlotConfig
      }
    | QueryTest
      { getAlgorithm :: SqlM (Entity Algorithm)
      , getPlatformId :: SqlM (Key Platform)
      , outputSuffix :: Maybe FilePath
      }

plotOptions :: PlotType -> Parser PlotOptions
plotOptions plottype =
  PlotOptions plottype <$> algorithmIdParser <*> platformIdParser <*> graphs
                       <*> impls <*> config
  where
    baseConfig = case plottype of
        PlotLevels -> pure $ PlotConfig "Levels" False
        PlotTotals -> PlotConfig "Graph" <$> normaliseFlag
        PlotVsOptimal -> PlotConfig "Graph" <$> normaliseFlag

    config :: Parser PlotConfig
    config = baseConfig <*> slideFlag <*> printFlag

    slideFlag :: Parser Bool
    slideFlag = flag False True $ mconcat
        [ long "slide", help "Render 4:3 slide dimensions" ]

    printFlag :: Parser Bool
    printFlag = flag False True $ mconcat
        [ long "print", help "Print results to stdout, rather than plotting" ]

    normaliseFlag :: Parser Bool
    normaliseFlag = flag False True $ mconcat [long "normalise"]

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

commands :: String -> Command PlotOptions
commands name = CommandGroup CommandInfo
  { commandName = name
  , commandHeaderDesc = "a tool for plotting benchmark results"
  , commandDesc = ""
  } [ SingleCommand CommandInfo
        { commandName = "levels"
        , commandHeaderDesc = "plot level times for a graph" 
        , commandDesc = ""
        } (plotOptions PlotLevels)
    , SingleCommand CommandInfo
        { commandName = "totals"
        , commandHeaderDesc = "plot total times for a set of graphs"
        , commandDesc = ""
        } (plotOptions PlotTotals)
    , SingleCommand CommandInfo
        { commandName = "vs-optimal"
        , commandHeaderDesc =
          "plot total times for a set of graphs against the optimal"
        , commandDesc = ""
        } (plotOptions PlotVsOptimal)
    , HiddenCommand CommandInfo
        { commandName = "query-test"
        , commandHeaderDesc = "check query output"
        , commandDesc = "Dump query output to files to validate results"
        }
        $ QueryTest <$> algorithmParser <*> platformIdParser
                    <*> (suffixParser <|> pure Nothing)
    ]
  where
    suffixReader :: String -> Maybe (Maybe String)
    suffixReader "" = Nothing
    suffixReader s
        | any isSpace s = Nothing
        | otherwise = Just $ Just s

    suffixParser :: Parser (Maybe String)
    suffixParser = argument (maybeReader suffixReader) . mconcat $
        [ metavar "SUFFIX" ]

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

runQueryDump
    :: (Foldable f, Show r)
    => Text -> Maybe String -> String -> (a -> Query r) -> f a -> SqlM ()
runQueryDump algoName Nothing _ mkQuery coll = case toList coll of
    (val:_) -> void $ runSqlQueryConduit (mkQuery val) await
    _ -> logThrowM $
        UnexpectedMissingData "No variants found for algorithm" algoName

runQueryDump _ (Just suffix) name mkQuery vals =
  withUnliftIO $ \(UnliftIO runInIO) ->
    withFile (name <> suffix) WriteMode $ \hnd ->
        forM_ vals $ \val -> runInIO . runSqlQueryConduit (mkQuery val) $
            C.map showText
            .| C.map (`T.snoc` '\n')
            .| C.encode C.utf8
            .| C.sinkHandle hnd

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
main = runSqlM commands $ \case
  PlotOptions{..} -> do
    algoId <- getAlgoId
    platformId <- getPlatformId
    implNames <- getImplementations
    variants <- getGraphs >>= queryVariants algoId

    impls <- (,) <$> Sql.queryImplementations algoId
                 <*> Sql.queryExternalImplementations algoId

    let implMaps@Pair{regular} =
          toImplNames (filterImpls implNames) (filterExternal implNames) impls

        translatePair :: Pair (Vector ImplTiming) -> Vector (Text, Double)
        translatePair x = mergePair $ nameImplementations <$> implMaps <*> x

    case plotType of
        PlotLevels -> do
            forM_ variants $ \variantId -> do
                graphId <- variantGraphId <$> Sql.getJust variantId
                name <- graphName <$> Sql.getJust graphId

                let query = levelTimePlotQuery platformId variantId
                    pdfName = name <> "-levels"

                plot plotConfig pdfName query $
                    C.map (bimap showText (nameImplementations regular))

        PlotTotals -> do
            let timeQuery = timePlotQuery algoId platformId variants

            plot plotConfig "times-totals" timeQuery $
                C.map (second $ translatePair . toPair V.convert V.convert)

        PlotVsOptimal -> do
            let variantQuery = variantInfoQuery algoId platformId
                variantFilter VariantInfo{variantId} =
                    S.member variantId variants

            plot plotConfig "times-vs-optimal" variantQuery $
                C.filter variantFilter
                .| C.mapM dataFromVariantInfo
                .| C.map (second $ translatePair . fmap V.convert)

  QueryTest{getAlgorithm,getPlatformId,outputSuffix} -> do
    Entity algoId algorithm <- getAlgorithm
    platformId <- getPlatformId
    variantConfigs <- Sql.selectKeysList
        [VariantConfigAlgorithmId ==. algoId]
        [Sql.Asc VariantConfigName]

    variants <- fmap concat . forM variantConfigs $ \cfgId ->
        Sql.selectKeysList
            [VariantVariantConfigId ==. cfgId]
            [Sql.Asc VariantGraphId]

    let algoName = getAlgoName algorithm

        chunkedVariants = map S.fromList $ chunksOf 500 variants
        timeQuery = timePlotQuery algoId platformId
        levelTimeQuery = levelTimePlotQuery platformId

    runQueryDump algoName outputSuffix "timeQuery-" timeQuery chunkedVariants
    runQueryDump algoName outputSuffix "levelTimeQuery-" levelTimeQuery variants
