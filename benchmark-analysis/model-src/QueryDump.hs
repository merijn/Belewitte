{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module QueryDump (modelQueryDump) where

import Data.Conduit (ConduitT, Void, (.|), runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

import Core
import FieldQuery (getDistinctFieldQuery)
import Query (Query, runSqlQuery, runSqlQueryConduit)
import Schema
import Sql (MonadSql, (==.))
import qualified Sql
import StepQuery
import VariantQuery

getConfigSet :: SqlM (Set (Key Algorithm, Key Platform, CommitId))
getConfigSet = runConduit $
    Sql.selectSource [] [] .| C.foldMap (S.singleton . toTuple)
  where
    toTuple :: Entity RunConfig -> (Key Algorithm, Key Platform, CommitId)
    toTuple (Entity _ RunConfig{..}) =
        (runConfigAlgorithmId, runConfigPlatformId, runConfigAlgorithmVersion)

toVariantInfoQuery
    :: (Key Algorithm, Key Platform, CommitId) -> Query VariantInfo
toVariantInfoQuery (algoId, platformId, commitId) =
  variantInfoQuery algoId platformId commitId Nothing

toStepInfoQueries
    :: (Key Algorithm, Key Platform, CommitId)
    -> ConduitT (Key Algorithm, Key Platform, CommitId)
                (Query StepInfo)
                SqlM
                ()
toStepInfoQueries (algoId, platformId, stepInfoCommit) = do
    query <- getDistinctFieldQuery GraphPropProperty
    stepInfoGraphProps <- runSqlQueryConduit query $ C.foldMap S.singleton

    stepInfoStepProps <- S.fromList . map (stepPropProperty . entityVal) <$>
        Sql.selectList [StepPropAlgorithmId ==. algoId] []

    stepInfoTimestamp <- liftIO getCurrentTime

    yield $ stepInfoQuery algoId platformId StepInfoConfig
                {stepInfoQueryMode = All, ..}
  where
    stepInfoSeed = 42

    stepInfoGraphs, stepInfoVariants, stepInfoSteps :: Percentage
    stepInfoGraphs = $$(validRational 1)
    stepInfoVariants = $$(validRational 1)
    stepInfoSteps = $$(validRational 1)

modelQueryDump :: FilePath -> SqlM ()
modelQueryDump outputSuffix = do
    configSet <- getConfigSet

    runConduit $
        C.yieldMany configSet
        .> streamQuery sortVariantTimings . toVariantInfoQuery
        .| querySink "variantInfoQuery-"

    runConduit $
        C.yieldMany configSet
        .> toStepInfoQueries
        .> streamQuery sortStepTimings
        .| querySink "stepInfoQuery-"
  where
    streamQuery
        :: (MonadExplain m, MonadLogger m, MonadSql m, MonadThrow m)
        => (r -> o) -> Query r -> ConduitT i o m ()
    streamQuery f query = runSqlQuery query (C.map f)

    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => FilePath -> ConduitT a Void m ()
    querySink  name =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile (name <> outputSuffix)
