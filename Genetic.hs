{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async
import Control.Monad.Trans
import Control.Monad.RWS
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
    }

type EvolveM = RWST EvolveState () [(Double, FilePath)] IO

allocFile :: MonadIO m => m FilePath
allocFile = liftIO $ do
    (path, hnd) <- openBinaryTempFile "./" "test.graph"
    hClose hnd
    return path

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . System.Directory.removeFile

genAddGraphs :: [a] -> (a -> IO (Double, FilePath)) -> EvolveM ()
genAddGraphs l f = liftIO (mapConcurrently f l) >>= modify . mappend

modifyM :: ([(Double, FilePath)] -> EvolveM [(Double, FilePath)]) -> EvolveM ()
modifyM f = get >>= f >>= put

runRVar :: RVar a -> EvolveM a
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

runProcWith :: MonadIO m => (ProcHandles -> m a) -> CreateProcess -> m a
runProcWith f process = do
    (hin, hout, herr, pid) <- liftIO $ createProcess process
    result <- f (hin, hout, herr)
    liftIO $ waitForProcess pid
    return result

runProc :: MonadIO m => CreateProcess -> m ()
runProc = runProcWith $ const (return ())

runAndGetFitness :: MonadIO m => CreateProcess -> m Double
runAndGetFitness = runProcWith $ \(_, Just hout, _) -> do
    [fitness, connectivity] <- map read . words <$> liftIO (hGetLine hout)
    return $ fitness * connectivity

computeFitness :: MonadIO m => FilePath -> m (Double, FilePath)
computeFitness graph = (,graph) <$> runAndGetFitness defaultProc {
    cmdspec = RawCommand "prun" ["-np", "1", "./evolve", "fitness", graph]
}

genGraph :: MonadIO m => Int -> Int -> m (Double, FilePath)
genGraph vertexCount edgeCount = do
    graph <- allocFile
    runProc defaultProc {
        cmdspec = RawCommand "prun" [ "-np", "1", "./gen-graph", "random"
                                    , graph , show vertexCount, show edgeCount]
    }
    computeFitness graph

crossover :: MonadIO m => FilePath -> FilePath -> FilePath -> m (Double, FilePath)
crossover graph1 graph2 output = (,output) <$> runAndGetFitness defaultProc {
    cmdspec = RawCommand "prun" ["-np", "1", "./evolve", "crossover", graph1,
                                 graph2, output]
}

addNewGraphs :: Int -> EvolveM ()
addNewGraphs n = do
    EvolveState{..} <- ask
    genAddGraphs [1..n] $ \_ -> genGraph vertexCount edgeCount

crossoverPairs :: EvolveM [(FilePath, FilePath)]
crossoverPairs = do
    n <- asks numChildren
    cdfMap <- cdfMapFromList <$> get
    runRVar . replicateM n $ do
        (remainder, x) <- weightedChoiceExtractCDF cdfMap
        (_, y) <- weightedChoiceExtractCDF remainder
        return (x, y)

trimPopulation :: EvolveM ()
trimPopulation = do
    n <- asks popSize
    names <- get >>= runRVar . weightedSample n
    modifyM . filterM $ \(_, path) -> do
        if path `elem` names
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


runEvolve :: EvolveState -> EvolveM a -> IO [(Double, FilePath)]
runEvolve s act = fst <$> execRWST act s []

main :: IO ()
main = do
    gen <- createSystemRandom
    let popSize = 100
        numChildren = 100
        vertexCount = 100
        edgeCount = 5050

    result <- runEvolve EvolveState{..} $ do
        liftIO $ putStrLn "Generated population!"
        addNewGraphs popSize
        get >>= liftIO . print . analyse . map fst

        replicateM_ 10 $ do
            pairs <- crossoverPairs
            genAddGraphs pairs $ \(f1, f2) -> allocFile >>= crossover f1 f2
            trimPopulation
            get >>= liftIO . print . analyse . map fst

    forM_ result $ \(fitness, path) -> do
        putStr $ path ++ ": "
        print fitness
