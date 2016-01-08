{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Random hiding (runRVar)
import qualified Data.Random
import Data.Random.Shuffle.Weighted
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
    , keepPercent :: Double
    }

type MonadEvolve m = (MonadMask m, MonadIO m, MonadReader EvolveState m)
type EvolveM = ReaderT EvolveState IO
type PopulationM = StateT [(Double, FilePath)] EvolveM

allocFile :: MonadIO m => m FilePath
allocFile = liftIO $ do
    (path, hnd) <- openBinaryTempFile "./" "test.graph"
    hClose hnd
    return path

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . System.Directory.removeFile

genAddGraphs :: [a] -> (a -> EvolveM (Double, FilePath)) -> PopulationM ()
genAddGraphs l f = do
    evolveState <- ask
    result <- liftIO $ mapConcurrently (\x -> runReaderT (f x) evolveState) l
    modify $ mappend result

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

runRVar :: RVar a -> PopulationM a
runRVar var = asks gen >>= liftIO . Data.Random.runRVar var

type ProcHandles = (Maybe Handle, Maybe Handle, Maybe Handle)

defaultProc :: CreateProcess
defaultProc = CreateProcess
      { cmdspec = ShellCommand "/usr/bin/env true"
      , cwd = Nothing
      , env = Nothing
      , std_in = Inherit
      , std_out = CreatePipe
      , std_err = Inherit
      , close_fds = True
      , create_group = False
      , delegate_ctlc = False
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
      }

runProcWith
    :: (MonadMask m, MonadIO m)
    => (ProcHandles -> m a)
    -> CreateProcess
    -> m a
runProcWith f process = do
    result <- try $ bracket
        (liftIO $ createProcess process)
        cleanup
        (\(hin, hout, herr, _) -> f (hin, hout, herr))

    case result of
        Left (SomeException _) -> runProcWith f process
        Right x -> return x
  where
    cleanup (hin, hout, herr, pid) = liftIO $ do
        mapM_ hClose hin
        mapM_ hClose hout
        mapM_ hClose herr
        waitForProcess pid

runProc :: (MonadMask m, MonadIO m) => CreateProcess -> m ()
runProc = runProcWith $ const (return ())

runAndGetFitness
    :: (MonadMask m, MonadIO m)
    => CreateProcess
    -> m Double
runAndGetFitness = runProcWith $ \(_, Just hout, _) -> do
    [fitness, connectivity] <- map read . words <$> liftIO (hGetLine hout)
    return $ fitness * connectivity

computeFitness
    :: (MonadMask m, MonadIO m)
    => FilePath
    -> m (Double, FilePath)
computeFitness graph = (,graph) <$> runAndGetFitness defaultProc {
    cmdspec = RawCommand "prun" ["-np", "1", "./evolve", "fitness", graph]
}

genGraph :: MonadEvolve m => m (Double, FilePath)
genGraph = do
    EvolveState{..} <- ask
    graph <- allocFile
    runProc defaultProc {
        cmdspec = RawCommand "prun" [ "-np", "1", "./gen-graph", "random"
                                    , graph , show vertexCount, show edgeCount]
    }
    computeFitness graph

crossover
    :: (MonadMask m, MonadIO m)
    => Double
    -> FilePath
    -> FilePath
    -> m (Double, FilePath)
crossover rate graph1 graph2 = do
    output <- allocFile
    (,output) <$> runAndGetFitness defaultProc {
    cmdspec = RawCommand "prun" ["-np", "1", "./evolve", "crossover",
                                 "--mutation-rate", show rate, graph1,
                                 graph2, output]
}

mutate :: (MonadMask m, MonadIO m) => Double -> FilePath -> m (Double, FilePath)
mutate rate graph = crossover rate graph graph

addNewGraphs :: Int -> PopulationM ()
addNewGraphs n = genAddGraphs [1..n] $ \_ -> genGraph

crossoverPairs :: PopulationM [(FilePath, FilePath)]
crossoverPairs = do
    n <- asks numChildren
    cdfMap <- cdfMapFromList <$> get
    runRVar . replicateM n $ do
        (remainder, x) <- weightedChoiceExtractCDF cdfMap
        (_, y) <- weightedChoiceExtractCDF remainder
        return (x, y)

trimPopulation :: PopulationM ()
trimPopulation = do
    EvolveState{..} <- ask
    let top = round $ fromIntegral popSize * keepPercent
    modify $ sortBy (flip $ comparing fst)
    best <- gets $ map snd . take top
    names <- gets (drop top) >>= runRVar . weightedSample (popSize - top)
    modifyM . filterM $ \(_, path) -> do
        if path `elem` (best ++ names)
           then return True
           else False <$ removeFile path

data Stats
    = Stats
    { minVal :: !Double
    , avgVal :: !Double
    , maxVal :: !Double
    , stdDev :: !Double
    } deriving (Show)

analyse :: [Double] -> Stats
analyse [] = error "Don't be a dumbass!"
analyse vs@(v:_) = go 0 v v 0 0 0 vs
  where
    go :: Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> Stats
    go n minV maxV total _ s [] = Stats minV (total / n) maxV (sqrt $ s / n)
    go !n !minV !maxV !total !m !s (x:xs) =
        go (n+1) (min minV x) (max maxV x) (total + x) nm newVar xs
       where
         newVar = s + delta * (x - nm)
         delta = x - m
         nm = m + delta/(n + 1)

runEvolution :: EvolveState -> PopulationM a -> IO [(Double, FilePath)]
runEvolution s act = runReaderT (execStateT act []) s

main :: IO ()
main = do
    gen <- createSystemRandom
    let popSize = 100
        numChildren = 100
        vertexCount = 100
        edgeCount = 5050
        mutationRate = 0.001
        keepPercent = 0.05

    result <- runEvolution EvolveState{..} $ do
        liftIO $ putStrLn "Generated population!"
        addNewGraphs popSize
        get >>= liftIO . print . analyse . map fst

        replicateM_ 10000 $ do
            pairs <- crossoverPairs
            genAddGraphs pairs . uncurry $ crossover mutationRate

            trimPopulation
            get >>= liftIO . print . analyse . map fst

    forM_ (sortBy (comparing fst) result) $ \(fitness, path) -> do
        putStr $ path ++ ": "
        print fitness
