{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (ErrorCall(..))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Random as Random
import Data.Random.Shuffle.Weighted
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Conc
import Options.Applicative
import qualified System.Directory (removeFile)
import System.IO
import System.Process
import System.Random.MWC (createSystemRandom, GenIO)

data EvolveConfig
    = EvolveConfig
    { gen :: GenIO
    , popSize :: Int
    , numChildren :: Int
    , vertexCount :: Int
    , edgeCount :: Int
    , mutationRate :: Double
    , keepPercent :: Double
    , numGenerations :: Int
    , initialPopulation :: [FilePath]
    }

type ProcHandles = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
type SlurmJob = ReaderT EvolveConfig (ReaderT Int IO)
type EvolveM = ReaderT EvolveConfig IO
type PopulationM = StateT (Set (Double, FilePath)) EvolveM
type MonadEvolve m = (MonadIO m, MonadReader EvolveConfig m, MonadThrow m)

allocFile :: MonadIO m => m FilePath
allocFile = liftIO $ do
    (path, hnd) <- openBinaryTempFile "./" "test.graph"
    hClose hnd
    return path

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . System.Directory.removeFile

withProcess
    :: (MonadIO m, MonadThrow m)
    => CreateProcess
    -> (ProcHandles -> IO b)
    -> m b
withProcess process getResult = go (5 :: Int)
  where
    showCmd (ShellCommand s) = s
    showCmd (RawCommand exe args) = unwords (exe:args)

    go 0 = throwM . ErrorCall $ "Failed to run: " ++ showCmd (cmdspec process)
    go n = do
        result <- liftIO . try $ bracket (createProcess process) cleanup work
        case result of
            Right v -> return v
            Left (SomeException e) -> do
                liftIO . hPutStrLn stderr $ show e
                go (n-1)

    work handles@(_, _, _, pid) = do
        result <- getResult handles
        liftIO $ waitForProcess pid
        return result

    cleanup (hin, hout, herr, pid) = do
        mapM_ hClose hin
        mapM_ hClose hout
        mapM_ hClose herr
        terminateProcess pid
        waitForProcess pid

execSlurmJob :: EvolveConfig -> Int -> SlurmJob a -> IO a
execSlurmJob cfg i job = runReaderT (runReaderT job cfg) i

runJob :: (String -> b) -> [String] -> SlurmJob b
runJob f args = do
    jid <- lift ask
    let process = (proc "srun" (srunArgs ++ args)){ std_out = CreatePipe }
        srunArgs = [ "-Q", "-N1", "-n1", "--ntasks-per-core=1", "--share"
                   , "--overcommit", "--jobid=" ++ show jid]
    withProcess process $ \case
        (_, Just hout, _, _) -> f <$> hGetLine hout
        _ -> throwM . ErrorCall $ "Could not read stdout"

runJob_ :: [String] -> SlurmJob ()
runJob_ = runJob (const ())

getFitness :: String -> (Double, FilePath)
getFitness s
    | [path, fitness, connectivity] <- words s = (read fitness * read connectivity, path)
    | otherwise = error $ "Incorrect output: " ++ s

computeFitness :: FilePath -> SlurmJob (Double, FilePath)
computeFitness graph = runJob getFitness ["./evolve", "fitness", graph]

genGraph :: SlurmJob (Double, FilePath)
genGraph = do
    EvolveConfig{..} <- ask
    graph <- allocFile
    runJob_ ["./gen-graph", "random", graph, show vertexCount, show edgeCount]
    computeFitness graph

crossover :: FilePath -> FilePath -> SlurmJob (Double, FilePath)
crossover graph1 graph2 = do
    EvolveConfig{..} <- ask
    output <- allocFile
    runJob getFitness [ "./evolve", "crossover", "--mutation-rate"
                      , show mutationRate, graph1, graph2, output]

runRVar :: Random.RVar a -> PopulationM a
runRVar var = asks gen >>= liftIO . Random.runRVar var

crossoverPairs :: PopulationM [(FilePath, FilePath)]
crossoverPairs = do
    n <- asks numChildren
    cdfMap <- cdfMapFromList . S.toList <$> get
    runRVar . replicateM n $ do
        (remainder, x) <- weightedChoiceExtractCDF cdfMap
        (_, y) <- weightedChoiceExtractCDF remainder
        return (x, y)

growPopulation :: [a] -> (a -> SlurmJob (Double, FilePath)) -> PopulationM ()
growPopulation jobs _ | null jobs = return ()
growPopulation jobs f = do
    config <- ask
    newIndividuals <- bracket (withProcess salloc getJobId) killAlloc $ \jid ->
        liftIO $ mapConcurrently (execSlurmJob config jid . f) jobs

    modify . S.union . S.fromList $ newIndividuals
  where
    salloc = (proc "salloc" sallocArgs){ std_err = CreatePipe }
    sallocArgs = [ "-n", show (length jobs), "--ntasks-per-core=1", "--share"
                 , "--no-shell"]

    getJobId :: ProcHandles -> IO Int
    getJobId (_, _, Just herr, _) = do
         (_:_:_:_:jid:_) <- words <$> hGetLine herr
         return $ read jid
    getJobId _ = throwM . ErrorCall $ "Couldn't read stderr!"

    killAlloc :: (MonadIO m, MonadThrow m) => Int -> m ()
    killAlloc i = withProcess (proc "scancel" [show i]) $ \(_,_,_,pid) ->
        void $ waitForProcess pid

filterPop :: (Eq a, MonadState (Set a) m) => (a -> m Bool) -> m ()
filterPop f = get >>= fmap S.fromAscList . filterM f . S.toAscList >>= put

trimPopulation :: PopulationM ()
trimPopulation = do
    EvolveConfig{..} <- ask
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

analysis :: PopulationM Stats
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

runEvolution :: EvolveConfig -> PopulationM a -> IO (Set (Double, FilePath))
runEvolution s act = runReaderT (execStateT act S.empty) s

evolveConfig :: GenIO -> Parser EvolveConfig
evolveConfig gen = EvolveConfig gen
    <$> option auto population
    <*> option auto children
    <*> option auto vertices
    <*> option auto edges
    <*> option auto mutations
    <*> option auto topIndividuals
    <*> option auto generations
    <*> many (strArgument initPopulation)
  where
    population = mconcat
        [ short 'p', long "population", value 100, metavar "SIZE"
        , help "Size of population", showDefault ]
    children = mconcat
        [ short 'c', long "children", value 100, metavar "NUM"
        , help "Number of children each generation", showDefault ]
    vertices = mconcat
        [ short 'v', long "vertices", metavar "NUM", help "Number of vertices" ]
    edges = mconcat
        [ short 'e', long "edges", metavar "NUM", help "Number of initial edges" ]
    mutations = mconcat
        [ short 'm', long "mutation-rate", value 0.001, metavar "PERCENT"
        , help "Mutation rate", showDefault ]
    topIndividuals = mconcat
        [ short 't', long "top-percent", value 0.05, metavar "PERCENT"
        , help "Top individuals", showDefault ]
    generations = mconcat
        [ short 'n', long "num-generations", value 1000, metavar "NUM"
        , help "Number of generations to run", showDefault ]
    initPopulation = mconcat
        [ metavar "GRAPHS", help "Initial population seed" ]

programOpts :: GenIO -> ParserInfo EvolveConfig
programOpts gen = info (helper <*> evolveConfig gen)
    ( fullDesc
    <> progDesc "Evolve graphs with uniform degree distribution using SLURM"
    <> header "genetic - generate graphs via evolutionary computing" )

main :: IO ()
main = do
    getNumProcessors >>= setNumCapabilities

    config@EvolveConfig{..} <- createSystemRandom >>= execParser . programOpts

    result <- runEvolution config $ do
        let newIndividuals = max 0 (popSize - length initialPopulation)

        growPopulation initialPopulation $ computeFitness
        growPopulation [1..newIndividuals] $ const genGraph
        trimPopulation

        analysis >>= liftIO . print

        replicateM_ numGenerations $ do
            pairs <- crossoverPairs
            growPopulation pairs $ uncurry crossover
            trimPopulation
            analysis >>= liftIO . print

    forM_ (S.toAscList result) $ \(fitness, path) -> do
        putStr $ path <> ": "
        print fitness
