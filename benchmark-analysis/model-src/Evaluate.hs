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

import Control.Monad.Fix (fix)
import Data.Bifunctor (second)
import Data.Conduit as C
import Data.Conduit.List as C (mapAccum)
import qualified Data.Conduit.Combinators as C
import Data.Function (on)
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
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as VU
import Numeric (showFFloat)

import Core
import FormattedOutput (renderOutput)
import Model
import Query
import Schema
import StepQuery (StepInfo(..))
import Sql (queryImplementations, queryExternalImplementations)
import Train
import Utils.Pair (Pair(..), mapFirst, mergePair)

percent :: Real n => n -> n -> Text
percent x y = T.pack $ showFFloat (Just 2) val "%"
  where
    val :: Double
    val = 100 * realToFrac x / realToFrac y

padText :: Word -> Text -> Text
padText n t = t <> T.replicate (fromIntegral n - T.length t) " "

liftTuple :: (a -> b -> c) -> (Int64, a) -> (Int64, b) -> (Int64, c)
liftTuple f (i1, v1) (i2, v2)
    | i1 == i2 = (i1, f v1 v2)
    | otherwise = error $ mconcat
        [ "Shouldn't happen! Found: " , show i1, " and ", show i2]

filterImpls :: Set ImplType -> IntMap Implementation -> IntMap Implementation
filterImpls implTypes = IM.filter (implFilter . implementationType)
  where
    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) implTypes

reportImplementations
    :: Unbox a
    => Int
    -> Pair (IntMap Text)
    -> ((Int64, a) -> (Int64, a) -> Ordering)
    -> (a -> Text)
    -> Pair (Vector (Int64, a))
    -> Text
reportImplementations pad implMaps cmp f =
    foldMap renderEntry
        . sortBy (cmp `on` snd)
        . mergePair
        . filterLabelEntries
        . fmap VU.toList
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
  , implTimes :: {-# UNPACK #-} !(Pair (Vector (Int64, Double)))
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

    let zeroTimeVec :: Vector (Int64, Double)
        zeroTimeVec = VU.map (second (const 0)) stepTimings `VU.snoc`
            (predictedImplId, 0)

        initial :: (Int, VariantAggregate)
        initial = (defaultImpl, VariantAgg
            { variantid = stepVariantId
            , optimalTime = 0
            , implTimes = Pair zeroTimeVec VU.empty
            })

        mapImpl :: Vector (Int64, Double) -> Vector (Int, Int)
        mapImpl = VU.imap (\i (impl, _) -> (fromIntegral impl, i))

        translateMap :: IntMap Int
        translateMap = IM.fromList . VU.toList .  mapImpl $ zeroTimeVec

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
            , implTimes = VU.zipWith (liftTuple (+)) <$> implTimes <*>
                    Pair (stepTimings `VU.snoc` newPrediction) VU.empty
            })
      where
        newPrediction :: (Int64, Double)
        newPrediction = (predictedImplId, getTime predictedImpl)

        predictedImpl :: Int
        predictedImpl
          | prediction == -1 = lastImpl
          | prediction < 0 = mispredictionStrategy lastImpl prediction
          | otherwise = prediction
          where
            prediction = predict model stepProps

        getTime :: Integral n => n -> Double
        getTime ix = snd $
            stepTimings `VU.unsafeIndex` (translateMap IM.! fromIntegral ix)

data TotalStatistics =
  TotalStats
  { variantCount :: {-# UNPACK #-} !Int
  , timesCumRelError :: {-# UNPACK #-} !(Pair (Vector (Int64, Double)))
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
    initial :: Unbox a => Pair (Vector (Int64, a)) -> TotalStatistics
    initial v = TotalStats
        { variantCount = 0
        , timesCumRelError = VU.map (second (const 0)) <$> v
        , relErrorOneToTwo = zeroIntVector
        , relErrorMoreThanFive = zeroIntVector
        , relErrorMoreThanTwenty = zeroIntVector
        , timesMaxRelError = zeroDoubleVector
        }
      where
        zeroIntVector = VU.map (const 0) <$> v
        zeroDoubleVector = VU.map (const 0) <$> v

    aggregate
        :: VariantAggregate -> TotalStatistics -> (TotalStatistics, Text)
    aggregate VariantAgg{..} TotalStats{..} = (, variantReport) TotalStats
          { variantCount = variantCount + 1
          , timesCumRelError =
                VU.zipWith (liftTuple (+)) <$> timesCumRelError <*> relTimings

          , relErrorOneToTwo =
                VU.zipWith (lessThan 2) <$> relTimings <*> relErrorOneToTwo

          , relErrorMoreThanFive =
                VU.zipWith (moreThan 5) <$> relTimings <*> relErrorMoreThanFive

          , relErrorMoreThanTwenty =
                VU.zipWith (moreThan 20) <$> relTimings
                                         <*> relErrorMoreThanTwenty

          , timesMaxRelError =
                VU.zipWith max <$> timesMaxRelError
                               <*> (VU.map snd <$> relTimings)
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

        lessThan :: Double -> (Int64, Double) -> Int -> Int
        lessThan x (_, val) count
            | val < x = count + 1
            | otherwise = count

        moreThan :: Double -> (Int64, Double) -> Int -> Int
        moreThan x (_, val) count
            | val > x = count + 1
            | otherwise = count

        relTimings = VU.map (second (/relToTime)) <$> implTimes

        findImplTime :: Int64 -> Maybe Double
        findImplTime i =
          snd <$> VU.find ((i==) . fst) (regular timesCumRelError)

        relToTime :: Double
        relToTime = fromMaybe (error "Implementation not found!") $
            case relTo of
                Optimal -> Just optimalTime
                Predicted -> findImplTime predictedImplId
                BestNonSwitching -> findImplTime bestNonSwitchingImplId

        relTiming t = percent t optimalTime <> " (" <> showText t <> ")\n"

        results = mapFirst (VU.cons (optimalImplId, optimalTime)) implTimes

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

        addBest v = VU.snoc v (bestNonSwitchingImplId, bestNonSwitchingTime)

        !bestNonSwitchingTime =
          VU.minimum
          . VU.map snd
          . VU.filter (isCoreImpl . fst)
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
        addBest v = VU.snoc v (bestNonSwitchingImplId, variantBestNonSwitching)

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

    reportTimings :: Pair (Vector (Int64, (Double, Int, Int, Int, Double)))
    reportTimings = VU.zipWith5 (\(i,a) b c d e-> (i,(a,b,c,d,e)))
            <$> timesCumRelError
            <*> relErrorOneToTwo
            <*> relErrorMoreThanFive
            <*> relErrorMoreThanTwenty
            <*> timesMaxRelError

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
