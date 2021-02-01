{-# LANGUAGE RecordWildCards #-}
module Query.Dump (plotQueryDump) where

import Data.Conduit (ConduitT, Void, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (chunksOf)
import qualified Data.Conduit.Text as C
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as Storable

import Core
import Query
import Query.Level (levelTimePlotQuery)
import Query.Time (timePlotQuery)
import Schema
import Sql (SelectOpt(Asc), (==.))
import qualified Sql
import Utils.ImplTiming

getConfigSet :: SqlM (Set (Key Algorithm, Key Platform, CommitId))
getConfigSet = Sql.selectSource [] [] $ C.foldMap (S.singleton . toTuple)
  where
    toTuple :: Entity RunConfig -> (Key Algorithm, Key Platform, CommitId)
    toTuple (Entity _ RunConfig{..}) =
        (runConfigAlgorithmId, runConfigPlatformId, runConfigAlgorithmVersion)

toTimeQueries
    :: (Key Algorithm, Key Platform, CommitId)
    -> ConduitT (Key Algorithm, Key Platform, CommitId)
                (Query ((Key Graph, Text)
                       , ( Storable.Vector ImplTiming
                         , Storable.Vector ImplTiming
                         )
                       )
                )
                (Region SqlM)
                ()
toTimeQueries (algoId, platformId, commitId) =
  Sql.selectKeysRegion [VariantAlgorithmId ==. algoId] [Asc VariantId]
  .| C.chunksOf 500
  .| C.map (timePlotQuery algoId platformId commitId . S.fromList)

toLevelTimeQueries
    :: (Key Platform, CommitId)
    -> ConduitT (Key Platform, CommitId)
                (Query (Int64, Storable.Vector ImplTiming))
                (Region SqlM)
                ()
toLevelTimeQueries (platformId, commitId) =
  Sql.selectKeysRegion [] [Asc VariantId]
  .| C.map (levelTimePlotQuery platformId commitId)

plotQueryDump :: FilePath -> SqlM ()
plotQueryDump outputSuffix = do
    configSet <- getConfigSet

    Sql.runRegionConduit $
        C.yieldMany configSet
        .> toTimeQueries
        .> streamQuery
        .| querySink "timeQuery-"

    Sql.runRegionConduit $
        C.yieldMany (S.map stripAlgorithm configSet)
        .> toLevelTimeQueries
        .> streamQuery
        .| querySink "levelTimeQuery-"
  where
    stripAlgorithm
        :: (Key Algorithm, Key Platform, CommitId) -> (Key Platform, CommitId)
    stripAlgorithm (_, platformId, commitId) = (platformId, commitId)

    querySink
        :: (MonadResource m, MonadThrow m, Show a)
        => FilePath -> ConduitT a Void m ()
    querySink  name =
        C.map showText
        .| C.map (`T.snoc` '\n')
        .| C.encode C.utf8
        .| C.sinkFile (name <> outputSuffix)
