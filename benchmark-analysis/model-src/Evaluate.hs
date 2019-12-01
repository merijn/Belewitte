{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Evaluate
    ( CompareReport
    , EvaluateReport
    , Report(..)
    , RelativeTo(..)
    , SortBy(..)
    , percent
    , evaluateModel
    , compareImplementations
    ) where

import Control.Applicative (ZipList(..))
import Control.Monad.Fix (fix)
import Data.Conduit as C
import Data.Conduit.List as C (mapAccum)
import qualified Data.Conduit.Combinators as C
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Any(..), (<>))
import Data.Ord (comparing)
import Data.Semigroup (Max, getMax)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Text as T
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as VS
import Numeric (showFFloat)

import Core
import FormattedOutput (renderOutput)
import Model
import Query
import Schema
import StepQuery (StepInfo(..))
import Sql (queryImplementations, queryExternalImplementations)
import Train
import Utils.ImplTiming
import Utils.Pair (Pair(..), mapFirst, mergePair)

percent :: Real n => n -> n -> Text
percent x y = T.pack $ showFFloat (Just 2) val "%"
  where
    val :: Double
    val = 100 * realToFrac x / realToFrac y

padText :: Word -> Text -> Text
padText n t = t <> T.replicate (fromIntegral n - T.length t) " "

liftImplTiming
    :: (Double -> Double -> Double) -> ImplTiming -> ImplTiming -> ImplTiming
liftImplTiming f (ImplTiming i1 v1) (ImplTiming i2 v2)
    | i1 == i2 = ImplTiming i1 (f v1 v2)
    | otherwise = error $ mconcat
        [ "Shouldn't happen! Found: " , show i1, " and ", show i2]

filterImpls :: Set ImplType -> IntMap Implementation -> IntMap Implementation
filterImpls implTypes = IM.filter (implFilter . implementationType)
  where
    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) implTypes

reportImplementations
    :: Int
    -> Pair (IntMap Text)
    -> ((Int64, a) -> (Int64, a) -> Ordering)
    -> (a -> Text)
    -> Pair [(Int64, a)]
    -> Text
reportImplementations pad implMaps cmp f =
    foldMap renderEntry
        . sortBy (cmp `on` snd)
        . mergePair
        . filterLabelEntries
  where
    lookupName :: IntMap Text -> (Int64, a) -> Maybe (Text, (Int64, a))
    lookupName implNames v@(i, _) = (,v) <$> implNames !? fromIntegral i

    filterLabelEntries :: Pair [(Int64, a)] -> Pair [(Text, (Int64, a))]
    filterLabelEntries entries = mapMaybe . lookupName <$> implMaps <*> entries

    padSize :: Word
    padSize = getMax $ 2 + mergePair (foldMap getLength <$> implMaps)
      where
        getLength :: Text -> Max Word
        getLength = Semigroup.Max . fromIntegral . T.length

    renderEntry (name, (_, val)) = mconcat
        [ T.replicate pad " ", padText padSize $ name <> ":", f val ]

foldGroup
    :: Monad m
    => (a -> a -> Bool)
    -> ConduitT a Void m b
    -> ConduitT a b m ()
foldGroup p sink = fix $ \loop -> await >>= \case
    Nothing -> return ()
    Just e -> do
        leftover e
        (C.takeWhile (p e) .| C.toConsumer sink) >>= yield
        loop

data VariantAggregate =
  VariantAgg
  { variantid :: {-# UNPACK #-} !(Key Variant)
  , optimalTime :: {-# UNPACK #-} !Double
  , implTimes :: {-# UNPACK #-} !(Pair (Vector ImplTiming))
  }

aggregateSteps
    :: (MonadLogger m, MonadThrow m)
    => Int
    -> (Int -> Int -> Int)
    -> Model
    -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps defaultImpl mispredictionStrategy model = do
    StepInfo{stepVariantId,stepTimings} <- C.peek >>= \case
        Just info -> return info
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one step input"

    let zerooutTiming :: ImplTiming -> ImplTiming
        zerooutTiming implTime = implTime { implTimingTiming = 0 }

        zeroTimeVec :: Vector ImplTiming
        zeroTimeVec = VS.map zerooutTiming stepTimings `VS.snoc`
            ImplTiming predictedImplId 0

        initial :: (Int, VariantAggregate)
        initial = (defaultImpl, VariantAgg
            { variantid = stepVariantId
            , optimalTime = 0
            , implTimes = Pair zeroTimeVec VS.empty
            })

        translateMap :: IntMap Int
        translateMap = VS.ifoldl' build IM.empty zeroTimeVec
          where
            build :: IntMap Int -> Int -> ImplTiming -> IntMap Int
            build implMap idx (ImplTiming impl _) =
                IM.insert (fromIntegral impl) idx implMap

    snd <$> C.foldl (aggregate translateMap) initial

  where
    aggregate
        :: IntMap Int
        -> (Int, VariantAggregate)
        -> StepInfo
        -> (Int, VariantAggregate)
    aggregate translateMap (!lastImpl, !VariantAgg{..}) !StepInfo{..} =
      (predictedImpl, VariantAgg
            { variantid = stepVariantId
            , optimalTime = optimalTime + getTime stepBestImpl
            , implTimes = VS.zipWith (liftImplTiming (+)) <$> implTimes <*>
                    Pair (stepTimings `VS.snoc` newPrediction) VS.empty
            })
      where
        newPrediction :: ImplTiming
        newPrediction = ImplTiming predictedImplId (getTime predictedImpl)

        predictedImpl :: Int
        predictedImpl
          | prediction == -1 = lastImpl
          | prediction < 0 = mispredictionStrategy lastImpl prediction
          | otherwise = prediction
          where
            prediction = predict model stepProps

        getTime :: Integral n => n -> Double
        getTime ix = implTimingTiming $
            stepTimings `VS.unsafeIndex` (translateMap IM.! fromIntegral ix)

data TotalStatistics =
  TotalStats
  { variantCount :: {-# UNPACK #-} !Int
  , timesCumRelError :: {-# UNPACK #-} !(Pair (Vector ImplTiming))
  , relErrorOneToTwo :: {-# UNPACK #-} !(Pair (Vector Int))
  , relErrorMoreThanFive :: {-# UNPACK #-} !(Pair (Vector Int))
  , relErrorMoreThanTwenty :: {-# UNPACK #-} !(Pair (Vector Int))
  , timesMaxRelError :: {-# UNPACK #-} !(Pair (Vector Double))
  }

aggregateVariants
    :: (MonadLogger m, MonadThrow m)
    => IntervalSet Int64
    -> RelativeTo
    -> Pair (IntMap Text)
    -> ConduitT VariantAggregate Text m TotalStatistics
aggregateVariants variantIntervals relTo implMaps = do
    VariantAgg{implTimes} <- C.peek >>= \case
        Just v -> return v
        Nothing -> logThrowM . PatternFailed $
            "Expected at least one aggregate result"

    C.mapAccum aggregate (initial implTimes)
  where
    initial :: Pair (Vector ImplTiming) -> TotalStatistics
    initial v = TotalStats
        { variantCount = 0
        , timesCumRelError = VS.map zerooutTiming <$> v
        , relErrorOneToTwo = zeroIntVector
        , relErrorMoreThanFive = zeroIntVector
        , relErrorMoreThanTwenty = zeroIntVector
        , timesMaxRelError = zeroDoubleVector
        }
      where
        zerooutTiming :: ImplTiming -> ImplTiming
        zerooutTiming implTime = implTime { implTimingTiming = 0 }

        zeroIntVector = VS.map (const 0) <$> v
        zeroDoubleVector = VS.map (const 0) <$> v

    aggregate
        :: VariantAggregate -> TotalStatistics -> (TotalStatistics, Text)
    aggregate VariantAgg{..} TotalStats{..} = (, variantReport) TotalStats
          { variantCount = variantCount + 1
          , timesCumRelError =
                VS.zipWith (liftImplTiming (+)) <$> timesCumRelError
                                                <*> relTimings

          , relErrorOneToTwo =
                VS.zipWith (lessThan 2) <$> relTimings <*> relErrorOneToTwo

          , relErrorMoreThanFive =
                VS.zipWith (moreThan 5) <$> relTimings <*> relErrorMoreThanFive

          , relErrorMoreThanTwenty =
                VS.zipWith (moreThan 20) <$> relTimings
                                         <*> relErrorMoreThanTwenty

          , timesMaxRelError =
                VS.zipWith max <$> timesMaxRelError
                               <*> (VS.map implTimingTiming <$> relTimings)
          }
      where
        shouldReportVariant :: Bool
        shouldReportVariant = fromSqlKey variantid `IS.member` variantIntervals

        variantReport :: Text
        variantReport = mIf shouldReportVariant $ mconcat
            [ "Variant #", showText (fromSqlKey variantid), "\n"
            , reportImplementations 4 implMaps comparison relTiming results
            , "\n"
            ]

        lessThan :: Double -> ImplTiming -> Int -> Int
        lessThan x (ImplTiming _ val) count
            | val < x = count + 1
            | otherwise = count

        moreThan :: Double -> ImplTiming -> Int -> Int
        moreThan x (ImplTiming _ val) count
            | val > x = count + 1
            | otherwise = count

        relTimings = VS.map makeRelative <$> implTimes
          where
            makeRelative (ImplTiming impl timing) =
                ImplTiming impl (timing / relToTime)

        findImplTime :: Int64 -> Maybe Double
        findImplTime i =
            implTimingTiming <$> VS.find compareImpl (regular timesCumRelError)
          where
            compareImpl (ImplTiming impl _) = impl == i

        relToTime :: Double
        relToTime = fromMaybe (error "Implementation not found!") $
            case relTo of
                Optimal -> Just optimalTime
                Predicted -> findImplTime predictedImplId
                BestNonSwitching -> findImplTime bestNonSwitchingImplId

        relTiming t = percent t optimalTime <> " (" <> showText t <> ")\n"

        results = unwrapImplTiming <$> mapFirst (VS.cons optimalTiming) implTimes
          where
            unwrapImplTiming = map (\(ImplTiming i v) -> (i, v)) . VS.toList
            optimalTiming = ImplTiming optimalImplId optimalTime

        comparison (i, v1) (j, v2)
            | i == optimalImplId = LT
            | j == optimalImplId = GT
            | otherwise = compare v1 v2

data RelativeTo = Optimal | Predicted | BestNonSwitching
    deriving (Eq,Ord,Show,Read)

data SortBy = Avg | Max
    deriving (Eq,Ord,Show,Read)

type EvaluateReport = Report (Set ImplType)
type CompareReport = Report (Any, Set ImplType)

data Report a = Report
     { reportVariants :: IntervalSet Int64
     , reportRelativeTo :: RelativeTo
     , reportSortBy :: SortBy
     , reportImplTypes :: a
     , reportDetailed :: Bool
     }

evaluateModel
    :: Entity Algorithm
    -> Key Platform
    -> Either Int Text
    -> EvaluateReport
    -> Model
    -> TrainingConfig
    -> SqlM ()
evaluateModel algo platId defImpl reportCfg@Report{..} model trainConfig =
  renderOutput $ do
    impls <- queryImplementations algoId
    let implMaps :: Pair (IntMap Text)
        implMaps = toImplNames (filterImpls reportImplTypes) id (impls, mempty)

        lookupByName :: Text -> Maybe Int
        lookupByName t = fmap fst
                       . listToMaybe
                       . filter ((t==) . implementationName . snd)
                       $ IM.toList impls

    defaultImpl <- case defImpl of
        Left i | IM.member i impls -> return i
        Right t | Just i <- lookupByName t -> return i
        _ -> logThrowM $ UnexpectedMissingData
                "Default implementation not found for algorithm"
                (getAlgoName algorithm)

    let aggregationConduit = aggregateSteps defaultImpl const model

    stats <- runSqlQuery query
      .| foldGroup ((==) `on` stepVariantId) aggregationConduit
      .| C.map (addBestNonSwitching impls)
      .| aggregateVariants reportVariants reportRelativeTo implMaps

    reportTotalStatistics reportCfg implMaps stats
  where
    Entity algoId algorithm = algo

    query = getTotalQuery algoId platId trainConfig

    addBestNonSwitching
        :: IntMap Implementation -> VariantAggregate -> VariantAggregate
    addBestNonSwitching impls VariantAgg{..} = VariantAgg
        { variantid = variantid
        , optimalTime = optimalTime
        , implTimes = mapFirst addBest implTimes
        }
      where
        isCoreImpl :: Int64 -> Bool
        isCoreImpl i = case IM.lookup (fromIntegral i) impls of
            Just Implementation{implementationType = Core} -> True
            _ -> False

        addBest v = v `VS.snoc`
            ImplTiming bestNonSwitchingImplId bestNonSwitchingTime

        !bestNonSwitchingTime =
          VS.minimum
          . VS.map implTimingTiming
          . VS.filter (isCoreImpl . implTimingImpl)
          . regular
          $ implTimes

compareImplementations
    :: Key Algorithm -> Key Platform -> CompareReport -> SqlM ()
compareImplementations algoId platformId cfg@Report{..} = renderOutput $ do
    impls <- (,) <$> queryImplementations algoId
                 <*> queryExternalImplementations algoId

    let implMaps :: Pair (IntMap Text)
        implMaps = toImplNames filterImplTypes filterExternalImpls impls

    stats <- runSqlQuery query
        .| C.map addBestNonSwitching
        .| aggregateVariants reportVariants reportRelativeTo implMaps

    reportTotalStatistics cfg implMaps stats
  where
    filterImplTypes = filterImpls implTypes
    filterExternalImpls
        | compareExternal = id
        | otherwise = const mempty

    query = variantInfoQuery algoId platformId
    (Any compareExternal, implTypes) = reportImplTypes

    addBestNonSwitching :: VariantInfo -> VariantAggregate
    addBestNonSwitching VariantInfo{..} = VariantAgg
        { variantid = variantId
        , optimalTime = variantOptimal
        , implTimes = mapFirst addBest $
                Pair variantTimings variantExternalTimings
        }
      where
        addBest v = v `VS.snoc`
            ImplTiming bestNonSwitchingImplId variantBestNonSwitching

reportTotalStatistics
    :: Monad m
    => Report a
    -> Pair (IntMap Text)
    -> TotalStatistics
    -> ConduitT () Text m ()
reportTotalStatistics Report{..} implMaps TotalStats{..} = do
    C.yield "Summarising:\n"
    C.yield $ reportImplementations 0 implMaps comparison relError reportTimings
  where
    relError (cumError, oneToTwo, gtFive, gtTwenty, maxError) = mconcat
        [ T.pack $ roundedVal (cumError / fromIntegral variantCount)
        , mIf reportDetailed $ mconcat
                [ "\t" <> percent oneToTwo variantCount
                , "\t" <> percent gtFive variantCount
                , "\t" <> percent gtTwenty variantCount
                ]
        , T.pack $ "\t" <> roundedVal maxError
        , "\n"
        ]
      where
        roundedVal val = showFFloat (Just 3) val ""

    vectorToZipList :: Storable a => Pair (Vector a) -> Compose Pair ZipList a
    vectorToZipList = Compose . fmap (ZipList . VS.toList)

    reportTimings :: Pair [(Int64, (Double, Int, Int, Int, Double))]
    reportTimings = decompose $ (\(ImplTiming i a) b c d e -> (i,(a,b,c,d,e)))
            <$> vectorToZipList timesCumRelError
            <*> vectorToZipList relErrorOneToTwo
            <*> vectorToZipList relErrorMoreThanFive
            <*> vectorToZipList relErrorMoreThanTwenty
            <*> vectorToZipList timesMaxRelError
      where
        decompose :: Compose Pair ZipList a -> Pair [a]
        decompose = fmap getZipList . getCompose

    comparison
        :: (Int64, (Double, Int, Int, Int, Double))
        -> (Int64, (Double, Int, Int, Int, Double))
        -> Ordering
    comparison = comparing compareTime
      where
        compareTime :: (Int64, (Double, Int, Int, Int, Double)) -> Double
        compareTime (_, (avgTime, _, _, _, maxTime)) = case reportSortBy of
            Avg -> avgTime
            Max -> maxTime
