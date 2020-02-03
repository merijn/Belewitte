{-# LANGUAGE RecordWildCards #-}
module QueryDump (modelQueryDump) where

import Data.Conduit (ConduitT, Void, (.|), runConduit)
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

toStepInfoQuery
    :: (Key Algorithm, Key Platform, CommitId) -> SqlM (Query StepInfo)
toStepInfoQuery (algoId, platformId, commitId) = do
    graphPropQuery <- getDistinctFieldQuery GraphPropProperty
    graphprops <- runSqlQueryConduit graphPropQuery $ C.foldMap S.singleton

    stepprops <- S.fromList . map (stepPropProperty . entityVal) <$>
        Sql.selectList [StepPropAlgorithmId ==. algoId] []

    ts <- liftIO getCurrentTime

    return $ stepInfoQuery algoId platformId commitId graphprops stepprops ts

modelQueryDump :: FilePath -> SqlM ()
modelQueryDump outputSuffix = do
    configSet <- getConfigSet

    runConduit $
        C.yieldMany configSet
        .> streamQuery sortVariantTimings . toVariantInfoQuery
        .| querySink "variantInfoQuery-"

    runConduit $
        C.yieldMany configSet
        .| C.mapM toStepInfoQuery
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
