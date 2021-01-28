{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Interesting where

import Data.Coerce (coerce)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Semigroup
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Core
import FormattedOutput (renderOutput)
import Query (runSqlQueryConduit)
import Query.Variant
import Schema
import qualified Sql
import Utils.ImplTiming
import Utils.Pair

type ImplFilter = IntMap Implementation -> IntMap Implementation

newtype ZipVector a = ZipVector { getZipVector :: Vector a } deriving Show

instance Functor ZipVector where
    fmap f (ZipVector v) = ZipVector $ V.map f v

instance Semigroup a => Semigroup (ZipVector a) where
    ZipVector v1 <> ZipVector v2 = ZipVector $ V.zipWith (<>) v1 v2

data InterestingVariants = Interest
    { bestVariants :: ZipVector (ArgMin Double (Text, Key Variant))
    , worstVariants :: ZipVector (ArgMax Double (Text, Key Variant))
    }
    deriving (Generic, Show)
    deriving Semigroup via (GenericSemigroupMonoid InterestingVariants)

propertiesFromVariant
    :: IntMap Text -> VariantInfo -> Maybe InterestingVariants
propertiesFromVariant implNames VariantInfo{..} = Just Interest
    { bestVariants = ZipVector $ coerce timings
    , worstVariants = ZipVector $ coerce timings
    }
  where
    timings :: Vector (Arg Double (Text, Key Variant))
    timings = V.mapMaybe lookupName $ V.convert variantTimings

    lookupName
        :: ImplTiming -> Maybe (Arg Double (Text, Key Variant))
    lookupName (ImplTiming implId timing) =
        mkArg <$> IM.lookup (fromIntegral implId) implNames
      where
        mkArg txt = Arg (timing / variantOptimal) (txt, variantId)

printInteresting
    :: Monad m => Int -> InterestingVariants -> ConduitT () Text m ()
printInteresting maxLen Interest{..} = do
    reportData "Best variant" . getZipVector $ getMin <$> bestVariants
    reportData "Worst variant" . getZipVector $ getMax <$> worstVariants
  where
    reportData
        :: Monad m
        => Text
        -> Vector (Arg v (Text, Key Variant))
        -> ConduitT () Text m ()
    reportData name vec = do
        C.yield $ name <> ":\n"
        V.forM_ vec $ \(Arg _ (implName, variantId)) -> do
            C.yield $ mconcat
                [ "    "
                , padName (implName <> ":")
                , showSqlKey variantId
                , "\n"
                ]
        C.yield "\n"

    padName :: Text -> Text
    padName t = t <> T.replicate (maxLen + 2 - T.length t) " "

filterVariantConfig :: Maybe (Key VariantConfig) -> VariantInfo -> SqlM Bool
filterVariantConfig Nothing _ = return True
filterVariantConfig (Just variantConfigId) VariantInfo{..} = do
    Variant{variantVariantConfigId} <- Sql.getJust variantId
    return $ variantVariantConfigId == variantConfigId

findInterestingVariants
    :: Maybe (Key VariantConfig) -> VariantInfoConfig -> ImplFilter -> SqlM ()
findInterestingVariants variantConfigId variantInfoConfig implFilter = do
    impls <- Sql.queryImplementations variantInfoAlgorithm

    let implMap :: IntMap Text
        implMap = regular $ toImplNames implFilter id (impls, mempty)

        maxLength :: Int
        maxLength = maximum $ T.length <$> implMap

    stats <- runSqlQueryConduit query $
        C.filterM (filterVariantConfig variantConfigId)
        .| C.foldMap (propertiesFromVariant implMap)

    case stats of
        Just v -> renderOutput $ printInteresting maxLength v
        Nothing -> logThrowM $ PatternFailed
            "Expected at least one variant with results!"
  where
    VariantInfoConfig{variantInfoAlgorithm} = variantInfoConfig

    query = variantInfoQuery variantInfoConfig
