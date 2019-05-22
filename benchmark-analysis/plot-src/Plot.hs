{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad (forM, forM_, void)
import Control.Monad.Fail (MonadFail)
import Data.Bifunctor (first, second)
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
import Database.Persist.Sqlite ((==.))
import qualified Database.Persist.Sqlite as Sql
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import System.IO (Handle, IOMode(WriteMode), hPutStr, stdout, withFile)

import Core
import OptionParsers
import PlotQuery (timePlotQuery, levelTimePlotQuery)
import ProcessUtils (withStdin)
import Query
import RuntimeData (getBarPlotScript)
import Schema

queryVariants :: Key Algorithm -> Set Text -> SqlM (Set (Key Variant))
queryVariants algoId graphs = do
    gids <- runConduitRes $ Sql.selectSource [] []
        .| C.filter (\i -> S.member (graphName (Sql.entityVal i)) graphs)
        .| C.map Sql.entityKey
        .| C.foldMap S.singleton
    variants <- forM (S.toList gids) $ \gId ->
        Sql.getBy $ UniqVariant gId algoId "default"

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
      , getImplementations :: Key Algorithm -> SqlM (IntMap Implementation)
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
    filterImpls textSet = IM.filter $ getAny . mconcat
        [ Any . (`S.member` textSet) . implementationName
        , Any . (Builtin==) . implementationType
        ]

commands :: String -> (InfoMod a, Parser PlotOptions)
commands name = (,) mempty . (<|>) hiddenCommands . hsubparser $ mconcat
    [ subCommand "levels" "plot level times for a graph" "" $
        plotOptions PlotLevels
    , subCommand "totals" "plot total times for a set of graphs" "" $
        plotOptions PlotTotals
    , subCommand "vs-optimal"
        "plot total times for a set of graphs against the optimal" "" $
        plotOptions PlotVsOptimal
    ]
  where
    subCommand :: String -> String -> String -> Parser a -> Mod CommandFields a
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ name ++ " " ++ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    hiddenCommands :: Parser PlotOptions
    hiddenCommands = hsubparser . mappend internal $
        subCommand "query-test" "check query output"
            "Dump query output to files to validate results" $
            QueryTest <$> algorithmParser <*> platformIdParser
                      <*> (suffixParser <|> pure Nothing)

    suffixReader :: String -> Maybe (Maybe String)
    suffixReader "" = Nothing
    suffixReader s
        | any isSpace s = Nothing
        | otherwise = Just $ Just s

    suffixParser :: Parser (Maybe String)
    suffixParser = argument (maybeReader suffixReader) . mconcat $
        [ metavar "SUFFIX" ]

reportData
    :: (MonadFail m, MonadIO m, MonadLogger m, MonadThrow m)
    => Handle
    -> Bool
    -> IntMap Implementation
    -> ConduitT (Text, Vector (Int64, Double)) Void m ()
reportData hnd normalise implMap = do
    Just (_, VU.map fst -> impls) <- C.peek
    liftIO . T.hPutStrLn hnd $ toColumnLabels impls
    C.mapM_ $ printGraph impls
  where
    translate :: Int64 -> Text
    translate i = getImplName $ implMap IM.! fromIntegral i

    toColumnLabels :: Vector Int64 -> Text
    toColumnLabels = mconcat . intersperse ":" . map translate . VU.toList

    printGraph
        :: (MonadIO m, MonadLogger m, MonadThrow m)
        => Vector Int64 -> (Text, Vector (Int64, Double)) -> m ()
    printGraph impls (graph, timingsPair)
        | mismatch = logThrowM $
            QueryResultMismatch (VU.toList impls) (VU.toList timingImpls)

        | otherwise = liftIO $ do
            T.hPutStr hnd $ graph <> " :"
            VU.forM_ processedTimings $ \time -> hPutStr hnd $ " " ++ show time
            T.hPutStrLn hnd ""
      where
        mismatch :: Bool
        mismatch = not . VU.and $ VU.zipWith (==) impls timingImpls

        timingImpls :: Vector Int64
        timings :: Vector Double
        (timingImpls, timings) = VU.unzip timingsPair

        maxTime :: Double
        maxTime = VU.maximum . VU.filter (not . isInfinite) $ timings

        processedTimings :: Vector Double
        processedTimings
            | normalise = VU.map (/ maxTime) timings
            | otherwise = timings

dataFromVariantInfo :: VariantInfo -> SqlM (Text, Vector (Int64,Double))
dataFromVariantInfo VariantInfo{..} = do
    graphId <- variantGraphId <$> Sql.getJust variantId
    name <- graphName <$> Sql.getJust graphId
    return (name, extendedTimings)
  where
    extendedTimings = variantTimings
        `VU.snoc` (bestNonSwitchingImplId, variantBestNonSwitching)
        `VU.snoc` (optimalImplId, variantOptimal)

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
        plotProcess <- getBarPlotScript args
        withStdin plotProcess doWithHandle
  where
    args :: [String]
    args = [T.unpack plotName, axisName, show normalise, show slideFormat]

    isRelevant :: (Int64, a) -> Bool
    isRelevant (i, _) = IM.member (fromIntegral i) impls

    doWithHandle :: Handle -> SqlM ()
    doWithHandle hnd = runSqlQuery query $
        convert
        .| C.map (second (VU.filter isRelevant))
        .| reportData hnd normalise impls

runQueryDump
    :: (Foldable f, Show r)
    => Text -> Maybe String -> String -> (a -> Query r) -> f a -> SqlM ()
runQueryDump algoName Nothing _ mkQuery coll = case toList coll of
    (val:_) -> runSqlQuery (mkQuery val) $ void await
    _ -> logThrowM $
        UnexpectedMissingData "No variants found for algorithm" algoName

runQueryDump _ (Just suffix) name mkQuery vals =
  withUnliftIO $ \(UnliftIO runInIO) ->
    withFile (name <> suffix) WriteMode $ \hnd ->
        forM_ vals $ \val ->
            runInIO . runSqlQuery (mkQuery val) $
                C.map showText
                .| C.map (`T.snoc` '\n')
                .| C.encode C.utf8
                .| C.sinkHandle hnd

main :: IO ()
main = runSqlM commands $ \case
  PlotOptions{..} -> do
    algoId <- getAlgoId
    platformId <- getPlatformId
    impls <- getImplementations algoId
    variants <- getGraphs >>= queryVariants algoId

    case plotType of
        PlotLevels -> do
            forM_ variants $ \variantId -> do
                graphId <- variantGraphId <$> Sql.getJust variantId
                name <- graphName <$> Sql.getJust graphId

                let query = levelTimePlotQuery platformId variantId
                    pdfName = name <> "-levels"

                plot plotConfig pdfName impls query $ C.map (first showText)

        PlotTotals -> do
            let timeQuery = timePlotQuery algoId platformId variants

            plot plotConfig "times-totals" impls timeQuery $
                awaitForever yield

        PlotVsOptimal -> do
            let variantQuery = variantInfoQuery algoId platformId
                variantFilter VariantInfo{variantId} =
                    S.member variantId variants

            plot plotConfig "times-vs-optimal" impls variantQuery $
                C.filter variantFilter .| C.mapM dataFromVariantInfo

  QueryTest{getAlgorithm,getPlatformId,outputSuffix} -> do
    Entity algoId algorithm <- getAlgorithm
    platformId <- getPlatformId
    variants <- Sql.selectKeysList [VariantAlgorithmId ==. algoId] []

    let algoName = getAlgoName algorithm

        chunkedVariants = map S.fromList $ chunksOf 500 variants
        timeQuery = timePlotQuery algoId platformId
        levelTimeQuery = levelTimePlotQuery platformId

    runQueryDump algoName outputSuffix "timeQuery-" timeQuery chunkedVariants
    runQueryDump algoName outputSuffix "levelTimeQuery-" levelTimeQuery variants
