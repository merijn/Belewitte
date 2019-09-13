{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Sql (module Sql.Core, module Sql) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

import Schema
import Sql.Core hiding (executeSql, liftProjectPersist)

queryExternalImplementations
    :: (MonadSql m, MonadUnliftIO m)
    => Key Algorithm -> m (IntMap ExternalImpl)
queryExternalImplementations algoId = runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ExternalImplAlgorithmId ==. aId ] []

    toIntMap :: Entity ExternalImpl -> IntMap ExternalImpl
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

queryImplementations
    :: (MonadSql m, MonadUnliftIO m)
    => Key Algorithm -> m (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

    mkImpl :: Text -> Text -> Implementation
    mkImpl short long = Implementation algoId short (Just long) Nothing Builtin

    builtinImpls :: IntMap Implementation
    builtinImpls = IM.fromList
        [ (predictedImplId, mkImpl "predicted" "Predicted")
        , (bestNonSwitchingImplId, mkImpl "best" "Best Non-switching")
        , (optimalImplId, mkImpl "optimal" "Optimal")
        ]
