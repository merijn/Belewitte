{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Query.Dump (modelQueryDump) where

import Data.Conduit (ConduitT, Void, (.|), yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

import Core
import Query (Query, streamQuery)
import Query.Train
import Query.Variant
import Schema
import Sql (Region)
import qualified Sql

getConfigSet :: SqlM (Set (Key Algorithm, Key Platform, CommitId))
getConfigSet = Sql.selectSource [] [] $ C.foldMap (S.singleton . toTuple)
  where
    toTuple :: Entity RunConfig -> (Key Algorithm, Key Platform, CommitId)
    toTuple (Entity _ RunConfig{..}) =
        (runConfigAlgorithmId, runConfigPlatformId, runConfigAlgorithmVersion)

toVariantInfoQuery
    :: UTCTime -> (Key Algorithm, Key Platform, CommitId) -> Query VariantInfo
toVariantInfoQuery ts (algoId, platformId, commitId) = variantInfoQuery $
    VariantInfoConfig algoId platformId commitId Nothing Nothing ts AllNewer False

toStepInfoQueries
    :: (Key Algorithm, Key Platform, CommitId)
    -> ConduitT (Key Algorithm, Key Platform, CommitId)
                (Query StepInfo)
                (Region SqlM)
                ()
toStepInfoQueries (stepInfoAlgorithm, stepInfoPlatform, stepInfoCommit) = do
    trainStepProps <- Sql.selectSource [] [] $
        C.foldMap (S.singleton . entityKey)

    stepInfoTimestamp <- liftIO getCurrentTime

    yield $ trainStepQuery TrainStepConfig
      { trainStepInfoConfig = StepInfoConfig{..}
      , trainStepQueryMode = Train
      , ..
      }

    yield $ trainStepQuery TrainStepConfig
      { trainStepInfoConfig = StepInfoConfig{..}
      , trainStepQueryMode = Validate
      , ..
      }
  where
    trainStepSeed = 42
    trainStepDatasets = mempty
    stepInfoFilterIncomplete = False
    stepInfoAllowNewer = AllNewer

    trainStepGraphs, trainStepVariants, trainStepSteps :: Percentage
    trainStepGraphs = $$(validRational 0.5)
    trainStepVariants = $$(validRational 0.5)
    trainStepSteps = $$(validRational 0.5)

modelQueryDump :: FilePath -> SqlM ()
modelQueryDump outputSuffix = do
    configSet <- getConfigSet
    utcTime <- liftIO $ getCurrentTime

    Sql.runRegionConduit $
        C.yieldMany configSet
        .| C.map (toVariantInfoQuery utcTime)
        .> streamQuery
        .| C.map sortVariantTimings
        .| querySink "variantInfoQuery-"

    Sql.runRegionConduit $
        C.yieldMany configSet
        .> toStepInfoQueries
        .> streamQuery
        .| C.map sortStepTimings
        .| querySink "stepInfoQuery-"
  where
    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => FilePath -> ConduitT a Void m ()
    querySink  name =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile (name <> outputSuffix)
