{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Catch (bracket)
import GHC.Conc (getNumProcessors)
import Network.Socket
import Options.Applicative hiding (Success)
import System.Posix.Signals hiding (Ignore)
import qualified Pipes.Prelude as P

import Process hiding (bracket)

data ClientConfig
    = ClientConfig
    { masterURI :: String
    , portID :: PortNumber
    , workerCount :: Int
    }

worker :: Input Task -> Output Reply -> SafeT IO ()
worker tasks results = runEffect $
    fromInput tasks >-> processPipe >-> toOutput results
  where
    processPipe = P.mapM $ \(Task i (exe:args)) -> do
        withProcess (proc exe args) $ \case
            Left (SomeException e) -> return . Result i . Failed $ displayException e
            Right s -> return . Result i $ Success s

programOpts :: ParserInfo ClientConfig
programOpts = info (helper <*> (ClientConfig <$> host <*> port <*> workers))
    ( fullDesc
    <> progDesc "Evolve graphs with uniform degree distribution using SLURM"
    <> header "genetic - generate graphs via evolutionary computing" )
  where
    host = option auto $ mconcat
        [ long "host", value "fs0", metavar "HOSTNAME"
        , help "Host to connect to", showDefault ]
    port = option (fromInteger <$> auto) $ mconcat
        [ long "port", value 1337, metavar "PORTNUM"
        , help "Port number", showDefault ]
    workers = option auto $ mconcat
        [ short 'n', long "num-workers", value maxBound, metavar "NUM"
        , help "Number of parallel workers", showDefault ]

main :: IO ()
main = withChildrenDo Kill $ do
    ClientConfig{..} <- execParser programOpts

    numWorkers <- min workerCount <$> getNumProcessors
    setNumCapabilities numWorkers

    tid <- myThreadId
    installHandler sigTERM (Catch $ killThread tid) Nothing

    (outTasks, inTasks) <- spawn unbounded
    (outResults, inResults) <- spawn unbounded

    let cleanup = Cleanup{ parent = Reraise, child = Ignore }
    send outResults . Request $ 2 * numWorkers
    replicateM_ numWorkers $ fork cleanup $ worker inTasks outResults
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        setSocketOption sock KeepAlive 1
        (addr:_) <- lookupAddr masterURI portID
        connect sock addr

        withSocket cleanup sock inResults outTasks
        waitForChildren
