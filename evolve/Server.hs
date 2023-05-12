{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Exception (ErrorCall(..))
import Control.Foldl (Fold)
import qualified Control.Foldl as Foldl
import Control.Monad
import qualified Control.Monad.Catch as Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import Data.Profunctor (lmap)
import qualified Data.Random as Random
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple (swap)
import GHC.Conc (getNumProcessors)
import Options.Applicative hiding (Success)
import Pipes.Lift
import qualified Pipes.Prelude as P
import qualified System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Random.MWC (createSystemRandom, GenIO)

import Process hiding (numChildren)
import WeightedShuffle

data CrossoverType = SinglePoint | VertexWise | EdgeWise deriving (Read, Show)

renderCrossoverFlag :: CrossoverType -> String
renderCrossoverFlag t = case t of
    SinglePoint -> "--single-point"
    VertexWise -> "--vertexwise"
    EdgeWise -> "--edgewise"

data Job = Job (forall m . MonadIO m => Result -> m ()) [String]

data EvolveConfig
    = EvolveConfig
    { gen :: GenIO
    , jobQueue :: Output Job
    , numWorkers :: Int
    , maxTime :: Maybe String
    , popSize :: Int
    , numChildren :: Int
    , vertexCount :: Int
    , edgeCount :: Int
    , mutationRate :: Double
    , keepPercent :: Double
    , numGenerations :: Int
    , crossoverType :: CrossoverType
    , initialPopulation :: [FilePath]
    }

type EvolveM = ReaderT EvolveConfig (SafeT IO)
type PopulationM = StateT (Set (Double, FilePath)) EvolveM

allocFile :: MonadIO m => m FilePath
allocFile = liftIO $ do
    (path, hnd) <- openBinaryTempFile "./" "test.graph"
    hClose hnd
    return path

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . System.Directory.removeFile

getFitness :: Text -> (Double, FilePath)
getFitness (T.unpack -> s)
    | [path, fitness, connectivity] <- words s = (read fitness * read connectivity, path)
    | otherwise = error $ "Incorrect output: " ++ s

computeFitness :: FilePath -> EvolveM [String]
computeFitness graph = return ["./evolve", "fitness", graph]

genGraph :: a -> EvolveM [String]
genGraph _ = do
    EvolveConfig{..} <- ask
    graph <- allocFile
    return ["./gen-graph", "random", graph, show vertexCount, show edgeCount]

crossover :: CrossoverType -> FilePath -> FilePath -> EvolveM [String]
crossover crossType graph1 graph2 = do
    EvolveConfig{..} <- ask
    output <- allocFile
    return [ "./evolve", "crossover", renderCrossoverFlag crossType
           , "--mutation-rate" , show mutationRate, graph1, graph2
           , output]

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

runJobs :: Foldable t => t a -> (a -> EvolveM [String]) -> Fold Text c -> PopulationM c
runJobs inputs foo resultFold = do
    EvolveConfig{..} <- ask
    bracket (spawn' unbounded) cleanup $ \(outResult, inResult, seal) -> do
        tvar <- liftIO $ newTVarIO 0
        let postResult r = liftIO $ do
                atomically $ modifyTVar' tvar (+1)
                send outResult r

        i <- lift $ Foldl.purely P.fold Foldl.length $
                each inputs >-> P.mapM foo >-> P.mapM (send jobQueue . Job postResult)

        liftIO . atomically $ do
            r <- readTVar tvar
            check (r == i)
            seal

        Foldl.purely P.fold resultFold $
            fromFiniteInput inResult >-> P.mapM checkError
  where
    cleanup :: (Output a, Input a, STM ()) -> IO ()
    cleanup (_, _, seal) = atomically seal

    checkError :: MonadThrow m => Result -> m Text
    checkError (Success r) = return r
    checkError (Failed e) = throwM $ ExecutionFailed e

growPopulation :: Foldable f => f a -> (a -> EvolveM [String]) -> PopulationM ()
growPopulation src conv = do
    result <- runJobs src conv $ lmap getFitness Foldl.set
    modify' $ S.union result

filterPop :: ((Double, FilePath) -> PopulationM Bool) -> PopulationM ()
filterPop f = gets S.toAscList >>= filterM f >>= put . S.fromAscList

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
runEvolution s act = runSafeT $ runReaderT (execStateT act S.empty) s

programOpts :: GenIO -> Output Job -> ParserInfo EvolveConfig
programOpts gen jobQ = info (helper <*> config)
    ( fullDesc
    <> progDesc "Evolve graphs with uniform degree distribution using SLURM"
    <> header "genetic - generate graphs via evolutionary computing" )
  where
    config :: Parser EvolveConfig
    config = EvolveConfig gen jobQ
        <$> option auto nodes
        <*> option auto runTime
        <*> option auto population
        <*> option auto children
        <*> option auto vertices
        <*> option auto edges
        <*> option auto mutations
        <*> option auto topIndividuals
        <*> option auto generations
        <*> option auto crossType
        <*> many (strArgument initPopulation)
      where
        nodes = mconcat
            [ short 'N', long "num-nodes", value 10, metavar "NUM"
            , help "Number of compute nodes", showDefault ]
        runTime = mconcat
            [ short 'T', long "max-time", value Nothing, metavar "TIME"
            , help "Max time per srun allocation", showDefault ]
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
        crossType = mconcat
            [ long "crossover-type", value EdgeWise, metavar "CROSSOVER"
            , help "Type of crossover to use", showDefault ]
        initPopulation = mconcat
            [ metavar "GRAPHS", help "Initial population seed" ]

taskDistributor
    :: (?threadState :: ThreadState)
    => Output Job
    -> IO (Consumer Reply (SafeT IO) a, Pipe Job Task (SafeT IO) b)
taskDistributor rerun = do
    tvar <- newTVarIO 0
    ref <- newIORef M.empty

    let cleanup :: IO ()
        cleanup = do
            tasks <- readIORef ref
            runSafeT . runEffect $ for (each tasks) $ send rerun

        registerJob :: (MonadIO m, MonadThrow m) => Reply -> m ()
        registerJob = \case
            Request i -> atomically $ modifyTVar' tvar (+i)
            Result i result -> do
                Job postResult _ <- deleteKey ref i
                postResult result
                atomically $ modifyTVar' tvar (+1)

        forward :: Pipe Job Task (SafeT IO) r
        forward = evalStateP 0 . forever $ do
            c <- atomically $ do
                count <- readTVar tvar
                check $ count > 0
                return count

            mask_ $ replicateM_ c $ do
                job@(Job _ cmd) <- await
                i <- increment
                liftIO . modifyIORef' ref $ M.insert i job
                yield $ Task i cmd

    return (P.mapM_ registerJob, forward `finally` cleanup)
  where
    increment :: MonadState Word m => m Word
    increment = state (\i -> let i' = 1+i in i' `seq` (i, i'))

    deleteKey :: (Ord k, MonadThrow m, MonadIO m) => IORef (M.Map k v) -> k -> m v
    deleteKey ref k = do
        val <- liftIO . atomicModifyIORef' ref . fmap swap $ M.updateLookupWithKey (\_ _ -> Nothing) k
        case val of
            Just v -> return v
            Nothing -> throwM (ErrorCall "missing id")

main :: IO ()
main = withChildrenDo Kill $ do
    genIO <- createSystemRandom
    (outJobs, inJobs) <- spawn unbounded

    config@EvolveConfig{..} <- execParser $ programOpts genIO outJobs

    getNumProcessors >>= setNumCapabilities

    Catch.bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        setSocketOption sock KeepAlive 1

        let resolveHints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrFamily = AF_INET
                , addrSocketType = Stream
                }

        (resolvedAddress:_) <- getAddrInfo (Just resolveHints) Nothing Nothing

        bind sock (addrAddress resolvedAddress)
        port <- socketPort sock
        listen sock 5

        fork Cleanup{ parent = Reraise, child = Kill } $ forever $ do
            bracketOnError (accept sock) (close . fst) $ \(s, _) -> liftIO $ do
                (oTasks, iTasks) <- spawn unbounded
                (oReplies, iReplies) <- spawn unbounded
                withSocket ignoreCleanup s iTasks oReplies

                (replySink, jobToTask) <- taskDistributor outJobs
                forkTwinEffects ignoreCleanup
                    (fromInput iReplies >-> replySink)
                    (fromInput inJobs >-> jobToTask >-> toOutput oTasks)

        fork Cleanup{ parent = Reraise, child = Ignore } $ do
            let time = case maxTime of
                    Just t -> ["-t", t]
                    Nothing -> []

                workerCount = (numChildren + numWorkers - 1) `div` numWorkers
                args =
                                ["-Q"
                                , "-N", show numWorkers
                                , "./dist/build/breeding-pool/breeding-pool"
                                , "--port" , show port
                                , "-n", show workerCount
                                ] ++ time
                process = proc "srun" args

            liftIO $ print args

            forever $ withProcess process $ \case
                    Left (SomeException e) -> throwM e
                    Right _ -> return ()

        result <- runEvolution config $ do
            let newIndividuals = max 0 (popSize - length initialPopulation)

            new <- runJobs [1..newIndividuals] genGraph $ lmap T.unpack Foldl.list
            growPopulation (initialPopulation ++ new) $ computeFitness
            trimPopulation

            liftIO $ putStr "Generation #0: "
            analysis >>= liftIO . print

            forM_ [1..numGenerations] $ \i -> do
                pairs <- crossoverPairs
                growPopulation pairs . uncurry $ crossover crossoverType
                trimPopulation
                analysis >>= \stats -> liftIO $ do
                    putStr $ "Generation #" ++ show i ++ ": "
                    print stats

        forM_ (S.toAscList result) $ \(fitness, path) -> do
            putStr $ path <> ": "
            print fitness

        killChildren
