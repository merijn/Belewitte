{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
    , evaluateModel
    , compareImplementations
    ) where

import Control.Applicative (ZipList(..))
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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Any(..))
import Data.Ord (comparing)
import Data.Semigroup (Max, getMax)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as VS
import Numeric (showFFloat)

import Core
import FormattedOutput (renderRegionOutput)
import Predictor
import Query
import Schema
import StepAggregate (VariantAggregate(..), stepAggregator)
import StepQuery
import qualified Sql
import Train
import Utils.ImplTiming
import Utils.Pair (Pair(..), mapFirst, mergePair)
import VariantQuery

padText :: Word -> Text -> Text
padText n t = t <> T.replicate (fromIntegral n - T.length t) " "

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
    :: IntervalSet Int64
    -> RelativeTo
    -> Pair (IntMap Text)
    -> ConduitT VariantAggregate Text (Region SqlM) TotalStatistics
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
            | otherwise = compare v1 v2 <> compare i j

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

evaluateModel :: [Predictor] -> EvaluateReport -> TrainingConfig -> SqlM ()
evaluateModel predictors reportCfg@Report{..} trainConfig =
  renderRegionOutput $ do
    impls <- filterImpls reportImplTypes <$>
        Sql.queryImplementations stepInfoAlgorithm

    predictorNames <- mapM (toPredictorName . predictorModelId) predictors

    let implMaps :: Pair (IntMap Text)
        implMaps = mapFirst (<> IM.fromList predictorNames) $
            toImplNames id id (impls, mempty)

    stats <- variantConduit
        .> streamQuery . stepInfoQuery stepCfg
        .| stepAggregator predictors
        .| C.map (addBestNonSwitching impls)
        .| aggregateVariants reportVariants reportRelativeTo implMaps

    reportTotalStatistics reportCfg implMaps stats
  where
    variantConduit :: ConduitT a (Key Variant) (Region SqlM) ()
    variantConduit = case mDatasets of
        Just datasets | not (S.null datasets) -> C.yieldMany datasets
            .> streamQuery . datasetVariantQuery stepInfoAlgorithm

        _ -> streamQuery (algorithmVariantQuery stepInfoAlgorithm)

    stepCfg@StepInfoConfig{..} = getStepInfoConfig trainConfig
    mDatasets = case trainConfig of
        LegacyTrainConfig LegacyConfig{..} -> legacyDatasets
        TrainConfig TrainStepConfig{..} -> trainStepDatasets

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

compareImplementations :: VariantInfoConfig -> CompareReport -> SqlM ()
compareImplementations variantInfoConfig cfg@Report{..} =
  renderRegionOutput $ do
    impls <- (,) <$> Sql.queryImplementations variantInfoAlgorithm
                 <*> Sql.queryExternalImplementations variantInfoAlgorithm

    let implMaps :: Pair (IntMap Text)
        implMaps = toImplNames filterImplTypes filterExternalImpls impls

    stats <- streamQuery query
        .| C.map addBestNonSwitching
        .| aggregateVariants reportVariants reportRelativeTo implMaps

    reportTotalStatistics cfg implMaps stats
  where
    VariantInfoConfig{variantInfoAlgorithm} = variantInfoConfig

    filterImplTypes = filterImpls implTypes

    filterExternalImpls
        | compareExternal = id
        | otherwise = const mempty

    query = variantInfoQuery variantInfoConfig
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
    comparison = comparing compareTime <> comparing fst
      where
        compareTime :: (Int64, (Double, Int, Int, Int, Double)) -> Double
        compareTime (_, (avgTime, _, _, _, maxTime)) = case reportSortBy of
            Avg -> avgTime
            Max -> maxTime
