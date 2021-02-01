{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Interesting where

import Control.Monad (forM_)
import Data.Bifunctor (bimap, second)
import Data.Coerce (coerce)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Foldable (foldMap')
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Semigroup
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Statistics.Sample (stdDev, mean, skewness, kurtosis)

import Core
import FormattedOutput (renderOutput)
import Query (runSqlQueryConduit)
import Query.Variant
import Schema
import qualified Sql
import Utils.ImplTiming
import Utils.Pair

newtype ZipVector a = ZipVector { getZipVector :: Vector a } deriving Show

newtype MonoidMap k v = MMap { getMonoidMap :: Map k v }

instance Functor (MonoidMap k) where
    fmap f (MMap m) = MMap $ f <$> m

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
    MMap m1 <> MMap m2 = MMap $ M.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
    mempty = MMap M.empty

instance Functor ZipVector where
    fmap f (ZipVector v) = ZipVector $ V.map f v

instance Semigroup a => Semigroup (ZipVector a) where
    ZipVector v1 <> ZipVector v2 = ZipVector $ V.zipWith (<>) v1 v2

data MinMaxPair a = MMPair { minVal :: !(Min a), maxVal :: !(Max a) }
    deriving (Foldable, Generic, Show)
    deriving Semigroup via (GenericSemigroupMonoid (MinMaxPair a))

type VariantVector f = ZipVector (f (Arg Double (Text, Key Variant)))
type VariantPair = MinMaxPair (Arg Double (Key Variant))

data InterestingVariants = Interest
    { bestVariantsOptimal :: {-# UNPACK #-} !(VariantVector Min)
    , worstVariantsOptimal :: {-# UNPACK #-} !(VariantVector Max)
    , bestVariants :: {-# UNPACK #-} !(VariantVector Min)
    , worstVariants :: {-# UNPACK #-} !(VariantVector Max)
    , difference :: {-# UNPACK #-} !VariantPair
    , normalisedSpread :: {-# UNPACK #-} !VariantPair
    , skewnessVal :: {-# UNPACK #-} !VariantPair
    , kurtosisVal :: {-# UNPACK #-} !VariantPair
    }
    deriving (Generic, Show)
    deriving Semigroup via (GenericSemigroupMonoid InterestingVariants)

getOrd :: Arg a b -> a
getOrd (Arg x _) = x

getVal :: Arg a b -> b
getVal (Arg _ x) = x

propertiesFromVariant
    :: IntMap Text -> VariantInfo -> Maybe InterestingVariants
propertiesFromVariant implNames VariantInfo{..} = Just Interest
    { bestVariantsOptimal = coerce relativeToOptimal
    , worstVariantsOptimal = coerce relativeToOptimal
    , bestVariants = coerce relativeToBest
    , worstVariants = coerce relativeToBest
    , difference = mkPair maxDifference
    , normalisedSpread = mkPair normalisedSpread
    , skewnessVal = mkPair $ skewness rawTimings
    , kurtosisVal = mkPair $ kurtosis rawTimings
    }
  where
    mkPair :: Double -> MinMaxPair (Arg Double (Key Variant))
    mkPair x = MMPair (Min arg) (Max arg)
      where
        arg = Arg x variantId

    maxDifference :: Double
    maxDifference = getOrd (V.maximum timings) / getOrd (V.minimum timings)

    normalisedSpread :: Double
    normalisedSpread = stdDev rawTimings / mean rawTimings

    relativeToOptimal :: Vector (Arg Double (Text, Key Variant))
    relativeToOptimal = V.map (normaliseArg variantOptimal) timings

    relativeToBest :: Vector (Arg Double (Text, Key Variant))
    relativeToBest = V.map (normaliseArg (getOrd (V.minimum timings))) timings

    timings :: Vector (Arg Double Text)
    timings = V.mapMaybe lookupName $ V.convert variantTimings

    rawTimings :: Vector Double
    rawTimings = V.map getOrd timings

    normaliseArg :: Double -> Arg Double v -> Arg Double (v, Key Variant)
    normaliseArg d = bimap (/ d) (, variantId)

    lookupName
        :: ImplTiming -> Maybe (Arg Double Text)
    lookupName (ImplTiming implId timing) = Arg timing <$>
        IM.lookup (fromIntegral implId) implNames

renderVariantKey :: Key Variant -> ConduitT a b SqlM Text
renderVariantKey variantId = lift $ do
    graphId <- variantGraphId <$> Sql.getJust variantId
    return $ showSqlKey variantId <> "\t(#" <> showSqlKey graphId <> ")"

printSummary :: InterestingVariants -> ConduitT () Text SqlM ()
printSummary Interest{..} = forM_ variantCounts $ \(variantId, count) -> do
    idTxt <- renderVariantKey variantId
    C.yield $ showText count <> ":\t" <> idTxt <> "\n"
  where
    mkMap :: Arg v (Key Variant) -> MonoidMap (Key Variant) (Sum Int)
    mkMap (Arg _ k) = MMap $ M.singleton k 1

    vectorToMap
        :: (x -> Arg Double (Text, Key Variant))
        -> ZipVector x
        -> MonoidMap (Key Variant) (Sum Int)
    vectorToMap f = V.foldMap' (mkMap . second snd . f) . coerce

    pairToMap
        :: MinMaxPair (Arg Double (Key Variant))
        -> MonoidMap (Key Variant) (Sum Int)
    pairToMap = foldMap' mkMap

    variantCounts :: [(Key Variant, Int)]
    variantCounts = sortBy (flip (comparing snd)) . M.toList . coerce $ mconcat
        [ vectorToMap getMin bestVariantsOptimal
        , vectorToMap getMax worstVariantsOptimal
        , vectorToMap getMin bestVariants
        , vectorToMap getMax worstVariants
        , pairToMap $ difference
        , pairToMap $ normalisedSpread
        , pairToMap $ skewnessVal
        , pairToMap $ kurtosisVal
        ]

printInteresting
    :: Int -> InterestingVariants -> ConduitT () Text SqlM ()
printInteresting maxLen Interest{..} = do
    reportData "Best variant (optimal)" . getZipVector $
        getMin <$> bestVariantsOptimal

    reportData "Worst variant (optimal)" . getZipVector $
        getMax <$> worstVariantsOptimal

    reportData "Best variant (non-switching)" . getZipVector $
        getMin <$> bestVariants

    reportData "Worst variant (non-switching)" . getZipVector $
        getMax <$> worstVariants

    reportPair "difference" $ difference
    reportPair "spread" $ normalisedSpread
    reportPair "skewness" $ skewnessVal
    reportPair "kurtosis" $ kurtosisVal
  where
    reportData
        :: Text
        -> Vector (Arg v (Text, Key Variant))
        -> ConduitT () Text SqlM ()
    reportData name vec = do
        C.yield $ name <> ":\n"
        V.forM_ vec $ \(Arg _ (implName, variantId)) -> do
            idTxt <- renderVariantKey variantId
            C.yield $ mconcat
                [ "    " , padName (implName <> ":") , idTxt , "\n" ]
        C.yield "\n"

    reportPair
        :: Text
        -> MinMaxPair (Arg a (Key Variant))
        -> ConduitT () Text SqlM ()
    reportPair label (MMPair (Min minArg) (Max maxArg)) = do
        render "Smallest" (getVal minArg)
        render "Largest" (getVal maxArg)
      where
        render :: Text -> Key Variant -> ConduitT () Text SqlM ()
        render t variantId = do
            idTxt <- renderVariantKey variantId
            C.yield $ mconcat
                [ padName (t <> " " <> label <> ":") , "    " , idTxt , "\n" ]

    padName :: Text -> Text
    padName t = t <> T.replicate (maxLen + 2 - T.length t) " "

data VariantFilter = VFilter
    { filterVariantConfigId :: Maybe (Key VariantConfig)
    , filterVertexSize :: Maybe Int
    , filterEdgeSize :: Maybe Int
    }

filterVariantConfig
    :: VariantFilter -> ConduitT VariantInfo VariantInfo SqlM ()
filterVariantConfig VFilter{..} = do
    checkVertices <- makeFilterFun "vertex count" filterVertexSize
    checkEdges <- makeFilterFun "edge count" filterVertexSize

    C.filterM $ \VariantInfo{..} -> do
        Variant{variantVariantConfigId,variantGraphId} <- Sql.getJust variantId
        if checkVariantConfig variantVariantConfigId
           then (&&) <$> checkVertices variantGraphId
                     <*> checkEdges variantGraphId
           else return False
  where
    checkVariantConfig :: Key VariantConfig -> Bool
    checkVariantConfig = case filterVariantConfigId of
        Nothing -> const True
        Just k -> (==k)

    makeFilterFun
        :: Text -> Maybe Int -> ConduitT a b SqlM (Key Graph -> SqlM Bool)
    makeFilterFun propName filterSize = case fromIntegral <$> filterSize of
        Nothing -> return $ const (return True)
        Just n -> do
            propId <- Sql.getJustKeyBy $ UniqProperty propName
            return $ \graphId -> do
                Entity _ graphProp <- Sql.getJustBy $
                    UniqGraphPropValue graphId propId

                return $ (>=n) . graphPropValueValue $ graphProp


findInterestingVariants
    :: VariantFilter
    -> VariantInfoConfig
    -> ImplFilter
    -> Bool
    -> SqlM ()
findInterestingVariants variantFilter variantInfoCfg implFilter summary = do
    impls <- Sql.queryImplementations variantInfoAlgorithm

    let implMap :: IntMap Text
        implMap = regular $ toImplNames implFilter id (impls, mempty)

        maxLength :: Int
        maxLength = maximum $ T.length <$> implMap

    stats <- runSqlQueryConduit query $
        filterVariantConfig variantFilter
        .| C.foldMap (propertiesFromVariant implMap)

    case stats of
        Just v | summary -> renderOutput $ printSummary v
               | otherwise -> renderOutput $ printInteresting maxLength v
        Nothing -> logThrowM $ PatternFailed
            "Expected at least one variant with results!"
  where
    VariantInfoConfig{variantInfoAlgorithm} = variantInfoCfg

    query = variantInfoQuery variantInfoCfg
