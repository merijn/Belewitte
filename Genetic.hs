{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.RWS hiding (reader)
import qualified Data.Random as Random
import Data.Random.Shuffle.Weighted
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Read (double)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Group as P
import qualified Pipes.Text as P
import Pipes.Text.IO
import qualified System.Directory (removeFile)
import System.IO
import System.Process
import System.Random.MWC (createSystemRandom, GenIO)

data EvolveState
    = EvolveState
    { gen :: GenIO
    , popSize :: Int
    , numChildren :: Int
    , vertexCount :: Int
    , edgeCount :: Int
    , mutationRate :: Double
    , keepPercent :: Double
    , numGenerations :: Int
    }

type Batchable t m = (Traversable t, MonadMask m, MonadIO m)
type EvolveM = RWST EvolveState () (Set (Double, Text)) IO

showText :: Show a => a -> Text
showText = T.pack . show

allocFile :: MonadIO m => m Text
allocFile = liftIO $ do
    (path, hnd) <- openBinaryTempFile "./" "test.graph"
    hClose hnd
    return $ T.pack path

removeFile :: MonadIO m => Text -> m ()
removeFile = liftIO . System.Directory.removeFile . T.unpack

runRVar :: (MonadIO m, MonadReader EvolveState m) => Random.RVar a -> m a
runRVar var = asks gen >>= liftIO . Random.runRVar var

runBatch :: Batchable t m => (Handle -> IO b) -> Int -> t Text -> m b
runBatch readOutput n jobs = do
    bracket launchProc cleanup $ \(hin, hout, _) -> do
        mapM_ (launchJob hin) jobs
        liftIO $ do
            hPutStrLn hin "wait"
            hClose hin
            readOutput hout
  where
    launchProc = liftIO $ do
        let process = proc "salloc"
                ["-Q", "-n", show n, "--ntasks-per-core=1", "--share"]
        (Just hin, Just hout, Nothing, pid) <- createProcess process
            { std_in = CreatePipe, std_out = CreatePipe }
        return (hin, hout, pid)

    cleanup (hin, hout, pid) = liftIO $ do
        hClose hin
        hClose hout
        waitForProcess pid

    launchJob hin s = liftIO $ do
        T.hPutStrLn hin $  mconcat
            [ "srun -Q -N1 -n1 --ntasks-per-core=1 --share --overcommit "
            , s, " 2> >(grep -v \"srun: cluster configuration lacks support for cpu binding\") &"
            ]

batchRun_ :: Batchable t m => t a -> (a -> m Text) -> m ()
batchRun_ coll f = mapM f coll >>= runBatch (const $ return ()) (length coll)

filterPop :: (Eq a, MonadState (Set a) m) => (a -> m Bool) -> m ()
filterPop f = get >>= fmap S.fromAscList . filterM f . S.toAscList >>= put

crossoverPairs :: EvolveM [(Text, Text)]
crossoverPairs = do
    n <- asks numChildren
    cdfMap <- cdfMapFromList . S.toList <$> get
    runRVar . replicateM n $ do
        (remainder, x) <- weightedChoiceExtractCDF cdfMap
        (_, y) <- weightedChoiceExtractCDF remainder
        return (x, y)

growPopulation :: Traversable t => t a -> (a -> EvolveM Text) -> EvolveM ()
growPopulation c f = do
    jobs <- mapM f c
    newIndividuals <- runBatch reader (length c) jobs
    modify . S.union . S.fromList $ newIndividuals
  where
    handleToLines = P.concats . view P.lines . fromHandle

    reader hout = P.toListM $ handleToLines hout >-> P.mapM fitnessResult

    fitnessResult :: Text -> IO (Double, Text)
    fitnessResult s
        | [path, fitness, connectivity] <- T.words s =
            return (toDouble fitness * toDouble connectivity, path)

        | otherwise = error $ "Shouldn't happen: " ++ show s
      where
        toDouble d = case double d of
            Right (v, _) -> v
            Left e -> error $ "Shouldn't happen: " ++ e

trimPopulation :: EvolveM ()
trimPopulation = do
    EvolveState{..} <- ask
    let top = round $ fromIntegral popSize * keepPercent
    best <- gets $ map snd . take top . S.toDescList
    remainder <- gets $ drop top . S.toDescList
    survivors <- runRVar . weightedSample (popSize - top) $ remainder
    filterPop $ cleanup (best ++ survivors)
  where
    cleanup names (_, path)
        | path `elem` names = return True
        | otherwise = False <$ removeFile path

data Stats
    = Stats
    { minVal :: !Double
    , avgVal :: !Double
    , maxVal :: !Double
    , stdDev :: !Double
    } deriving (Show)

analysis :: EvolveM Stats
analysis = do
    vs@(v:_) <- gets $ map fst . S.toList
    return $! go 0 v v 0 0 0 vs
  where
    go :: Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> Stats
    go n minV maxV total _ s [] = Stats minV (total / n) maxV (sqrt $ s / n)
    go !n !minV !maxV !total !m !s (x:xs) =
        go (n+1) (min minV x) (max maxV x) (total + x) nm newVar xs
       where
         newVar = s + delta * (x - nm)
         delta = x - m
         nm = m + delta/(n + 1)

runEvolution :: EvolveState -> EvolveM a -> IO (Set (Double, Text))
runEvolution s act = fst <$> execRWST act s S.empty

{-
 - showDefault :: Show a => Mod f a Source
 - noArgError :: ParseError -> Mod OptionFields a
 -}

evolveState :: GenIO -> Parser EvolveState
evolveState gen = EvolveState gen
    <$> option auto population
    <*> option auto children
    <*> option auto vertices
    <*> option auto edges
    <*> option auto mutations
    <*> option auto topIndividuals
    <*> option auto generations
  where
    population = mconcat
        [ short 'p', long "population", value 100, metavar "SIZE"
        , help "Size of population" ]
    children = mconcat
        [ short 'c', long "children", value 100, metavar "NUM"
        , help "Number of children each generation" ]
    vertices = mconcat
        [ short 'v', long "vertices", metavar "NUM", help "Number of vertices" ]
    edges = mconcat
        [ short 'e', long "edges", metavar "NUM", help "Number of initial edges" ]
    mutations = mconcat
        [ short 'm', long "mutation-rate", value 0.001, metavar "PERCENT"
        , help "Mutation rate" ]
    topIndividuals = mconcat
        [ short 't', long "top-percent", value 0.05, metavar "PERCENT"
        , help "Top individuals" ]
    generations = mconcat
        [ short 'n', long "num-generations", value 1000, metavar "NUM"
        , help "Number of generations to run" ]

programOpts :: GenIO -> ParserInfo EvolveState
programOpts gen = info (helper <*> evolveState gen)
    ( fullDesc
    <> progDesc "Evolve graphs with uniform degree distribution using SLURM"
    <> header "genetic - generate graphs via evolutionary computing" )

main :: IO ()
main = do
    config@EvolveState{..} <- createSystemRandom >>= execParser . programOpts

    result <- runEvolution config $ do
        newGraphs <- replicateM popSize allocFile

        batchRun_ newGraphs $ \graph -> do
            return . T.unwords $ [ "./gen-graph", "random", graph
                                 , showText vertexCount, showText edgeCount]

        growPopulation newGraphs $ \graph ->
            return . T.unwords $ ["./evolve", "fitness", graph]

        liftIO $ putStrLn "Generated population!"
        analysis >>= liftIO . print

        replicateM_ numGenerations $ do
            pairs <- crossoverPairs
            growPopulation pairs $ \(graph1, graph2) -> do
                output <- allocFile
                return . T.unwords $
                    ["./evolve", "crossover", "--mutation-rate"]
                    ++ [showText mutationRate, graph1, graph2, output]

            trimPopulation
            analysis >>= liftIO . print

    forM_ (S.toAscList result) $ \(fitness, path) -> do
        T.putStr $ path <> ": "
        print fitness
