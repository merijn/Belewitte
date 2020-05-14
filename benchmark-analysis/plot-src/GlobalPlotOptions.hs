{-# LANGUAGE ApplicativeDo #-}
module GlobalPlotOptions (GlobalPlotOptions(..), globalOptionsParser) where

import Data.IntMap (IntMap)

import Core
import Options
import Schema
import qualified Sql

data GlobalPlotOptions
    = GlobalPlotOptions
    { globalPlotAlgorithm :: Key Algorithm
    , globalPlotPlatform :: Key Platform
    , globalPlotCommit :: CommitId
    , globalPlotImpls :: (IntMap Implementation, IntMap ExternalImpl)
    }

globalOptionsParser :: Parser (SqlM GlobalPlotOptions)
globalOptionsParser = do
    getAlgoId <- algorithmIdParser
    getPlatformId <- platformIdParser
    getCommit <- commitIdParser

    pure $ do
        algoId <- getAlgoId
        impls <- (,) <$> Sql.queryImplementations algoId
                     <*> Sql.queryExternalImplementations algoId

        GlobalPlotOptions algoId
            <$> getPlatformId <*> getCommit algoId <*> pure impls
