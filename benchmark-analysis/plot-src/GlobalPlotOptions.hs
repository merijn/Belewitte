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
    , globalPlotTimestamp :: UTCTime
    , globalPlotAllowNewer :: AllowNewer
    , globalPlotImpls :: (IntMap Implementation, IntMap ExternalImpl)
    , globalPlotImplRenames :: IntMap Text
    }

globalOptionsParser :: Parser (SqlM GlobalPlotOptions)
globalOptionsParser = do
    getAlgoId <- algorithmIdParser
    getPlatformId <- platformIdParser
    getCommit <- commitIdParser
    getUtcTime <- requiredUtcTimeParser
    allowNewer <- allowNewerParser
    filterImpls <- intMapFilter "impl-set" "implementation" <|> pure id
    filterExtImpls <- intMapFilter "ext-impl-set" "external implementation"
                        <|> pure (const mempty)
    renames <- renameParser

    pure $ do
        algoId <- getAlgoId
        impls <- filterImpls <$> Sql.queryImplementations algoId
        extImpls <- filterExtImpls <$> Sql.queryExternalImplementations algoId

        GlobalPlotOptions algoId
            <$> getPlatformId <*> getCommit algoId <*> getUtcTime
            <*> pure allowNewer <*> pure (impls, extImpls) <*> pure renames
