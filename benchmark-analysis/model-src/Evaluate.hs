{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Evaluate
    ( CompareReport
    , EvaluateReport
    , Report(..)
    , Splittable(..)
    , ReportOutput(..)
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
import qualified Data.String.Interpolate.IsString as Interpolate
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
import Query.Step
import Query.Variant
import qualified Sql
import Utils.ImplTiming
import Utils.Pair (Pair(..), mapFirst, mergePair)

padText :: Word -> Text -> Text
padText n t = t <> T.replicate (fromIntegral n - T.length t) " "

reportImplementations
    :: Text
    -> Int
    -> Pair (IntMap Text)
    -> ((Int64, a) -> (Int64, a) -> Ordering)
    -> (a -> Text)
    -> Pair [(Int64, a)]
    -> Text
reportImplementations sep pad implMaps cmp f =
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
        [ T.replicate pad " ", padText padSize $ name <> sep, f val ]

data TotalStatistics =
  TotalStats
  { variantCount :: {-# UNPACK #-} !Int
  , aggregateComparisonTime :: {-# UNPACK #-} !Double
  , aggregateTimes :: {-# UNPACK #-} !(Pair (Vector ImplTiming))
  , timesCumRelError :: {-# UNPACK #-} !(Pair (Vector Double))
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
        , aggregateComparisonTime = 0
        , aggregateTimes = VS.map zerooutTiming <$> v
        , timesCumRelError = zeroDoubleVector
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
          , aggregateComparisonTime = aggregateComparisonTime + relToTime
          , aggregateTimes =
                VS.zipWith (liftImplTiming (+)) <$> aggregateTimes
                                                <*> implTimes
          , timesCumRelError =
                VS.zipWith add <$> timesCumRelError <*> relTimings

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
            , reportImplementations ":" 4 implMaps comparison relTiming results
            , "\n"
            ]

        add :: Double -> ImplTiming -> Double
        add x (ImplTiming _ v) = x + v

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
            implTimingTiming <$> VS.find compareImpl (regular implTimes)
          where
            compareImpl (ImplTiming impl _) = impl == i

        relToTime :: Double
        relToTime = fromMaybe (error "Implementation not found!") $
            case relTo of
                Optimal -> Just optimalTime
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

data RelativeTo = Optimal | BestNonSwitching
    deriving (Eq,Ord,Show,Read)

data SortBy = AvgError | MaxError | AbsTime
    deriving (Eq,Ord,Show,Read)

data Splittable = Splittable | Fixed
    deriving (Eq,Ord,Show,Read)

data ReportOutput = Minimal | Detailed | LaTeX Text Splittable
    deriving (Eq,Ord,Show,Read)

type EvaluateReport = Report ImplFilter
type CompareReport = Report (Any, ImplFilter)

data Report a = Report
     { reportVariants :: IntervalSet Int64
     , reportRelativeTo :: RelativeTo
     , reportSortBy :: SortBy
     , reportImplFilter :: a
     , reportOutput :: ReportOutput
     }

evaluateModel
    :: [RawPredictor]
    -> EvaluateReport
    -> StepInfoConfig
    -> Set (Key Dataset)
    -> SqlM ()
evaluateModel predictors reportCfg stepCfg datasets = do
  renderRegionOutput $ do
    impls <- reportImplFilter <$> Sql.queryImplementations stepInfoAlgorithm
    predictorNames <- mapM (toPredictorName . rawPredictorId) predictors

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
    Report{..} = reportCfg
    StepInfoConfig{..} = stepCfg

    variantConduit :: ConduitT a (Key Variant) (Region SqlM) ()
    variantConduit
        | not (S.null datasets) = C.yieldMany datasets
            .> streamQuery . datasetVariantQuery stepInfoAlgorithm

        | otherwise = streamQuery (algorithmVariantQuery stepInfoAlgorithm)

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
        implMaps = toImplNames implFilter filterExternalImpls impls

    stats <- streamQuery query
        .| C.concatMap addBestNonSwitching
        .| aggregateVariants reportVariants reportRelativeTo implMaps

    reportTotalStatistics cfg implMaps stats
  where
    VariantInfoConfig{variantInfoAlgorithm} = variantInfoConfig

    filterExternalImpls
        | compareExternal = id
        | otherwise = const mempty

    query = variantInfoQuery variantInfoConfig
    (Any compareExternal, implFilter) = reportImplFilter

    addBestNonSwitching :: VariantInfo -> Maybe VariantAggregate
    addBestNonSwitching VariantInfo{..}
        | compareExternal && VS.null variantExternalTimings = Nothing
        | otherwise = Just VariantAgg
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
reportTotalStatistics Report{..} implMaps TotalStats{..} = case reportOutput of
    LaTeX label splittable -> do
        C.yield $ latexTableHeader label splittable

        runReport (fmap latexEscape <$> implMaps) " &" $
            \(absTime, cumError, oneToTwo, gtFive, gtTwenty, maxError) ->
                mconcat
                    [ "$", roundVal (absTime / aggregateComparisonTime)
                    , "{\\times}$ & $"
                    , roundVal (cumError / fromIntegral variantCount)
                    , "{\\times}$ & "
                    , latexPercent oneToTwo variantCount, " & "
                    , latexPercent gtFive variantCount, " & "
                    , latexPercent gtTwenty variantCount, " & $"
                    , roundVal maxError
                    , "{\\times}$\\\\\n"
                    ]

        C.yield $ latexTableFooter label splittable

    Minimal -> do
        C.yield "Summarising:\n"
        runReport implMaps ":" $ \(absTime, cumError, _, _, _, maxError) ->
            mconcat
                [ roundVal (absTime / aggregateComparisonTime), "\t"
                , roundVal (cumError / fromIntegral variantCount), "\t"
                , roundVal maxError, "\n"
                ]

    Detailed -> do
        C.yield "Summarising:\n"
        runReport implMaps ":" $
            \(absTime, cumError, oneToTwo, gtFive, gtTwenty, maxError) ->
                mconcat
                    [ roundVal (absTime / aggregateComparisonTime), "\t"
                    , roundVal (cumError / fromIntegral variantCount)
                    , "\t", percent oneToTwo variantCount, "\t"
                    , percent gtFive variantCount, "\t"
                    , percent gtTwenty variantCount
                    , roundVal maxError
                    , "\n"
                    ]
  where
    runReport
        :: Monad m
        => Pair (IntMap Text)
        -> Text
        -> ((Double, Double, Int, Int, Int, Double) -> Text)
        -> ConduitT () Text m ()
    runReport names sep reportFun = C.yield $
        reportImplementations sep 0 names comparison reportFun reportTimings

    roundVal :: Double -> Text
    roundVal val = T.pack $ showFFloat (Just 2) val ""

    latexPercent :: Real n => n -> n -> Text
    latexPercent x y = T.pack $ '$' : showFFloat (Just 0) val "\\%$"
      where
        val :: Double
        val = 100 * realToFrac x / realToFrac y

    vectorToZipList :: Storable a => Pair (Vector a) -> Compose Pair ZipList a
    vectorToZipList = Compose . fmap (ZipList . VS.toList)

    reportTimings :: Pair [(Int64, (Double, Double, Int, Int, Int, Double))]
    reportTimings = decompose $ (\(ImplTiming i a) b c d e f -> (i,(a,b,c,d,e,f)))
            <$> vectorToZipList aggregateTimes
            <*> vectorToZipList timesCumRelError
            <*> vectorToZipList relErrorOneToTwo
            <*> vectorToZipList relErrorMoreThanFive
            <*> vectorToZipList relErrorMoreThanTwenty
            <*> vectorToZipList timesMaxRelError
      where
        decompose :: Compose Pair ZipList a -> Pair [a]
        decompose = fmap getZipList . getCompose

    comparison
        :: (Int64, (Double, Double, Int, Int, Int, Double))
        -> (Int64, (Double, Double, Int, Int, Int, Double))
        -> Ordering
    comparison = comparing compareTime <> comparing fst
      where
        compareTime
            :: (Int64, (Double, Double, Int, Int, Int, Double)) -> Double
        compareTime (_, (absTime, cumError, _, _, _, maxError)) =
          case reportSortBy of
            AvgError -> cumError -- Same number of variants for every data
                                 -- point so cumulative is same order as
                                 -- average
            MaxError -> maxError
            AbsTime -> absTime

    latexEscape :: Text -> Text
    latexEscape = T.replace "%" "\\%"

latexCaption :: Text -> Text
latexCaption label = [Interpolate.i|\\caption{\\glsdesc*{#{label}}}\\label{#{label}}%|]

latexTableHeader :: Text -> Splittable -> Text
latexTableHeader label splittable = case splittable of
    Splittable -> [Interpolate.i|\\begin{longtable}{#{columnLayout}}
\\toprule
#{columnHeader}
\\endfirsthead%
\\toprule
#{columnHeader}
\\endhead%
\\bottomrule
\\endfoot%
\\bottomrule
#{latexCaption label}
\\endlastfoot%
|]

    Fixed -> [Interpolate.i|\\begin{table}
\\centering
\\begin{tabular}{#{columnLayout}}
\\toprule
#{columnHeader}
|]
  where
    columnLayout :: Text
    columnLayout = "lrrrrrrr"

    columnHeader :: Text
    columnHeader = [Interpolate.i|Algorithm & Total & Avg & 1--2${\\times}$ & ${>} 5 {\\times}$ &
${>} 20 {\\times}$ &  Worst\\\\\\midrule|]

latexTableFooter :: Text -> Splittable -> Text
latexTableFooter _ Splittable = "\\end{longtable}\n"
latexTableFooter label Fixed = [Interpolate.i|\\bottomrule
\\end{tabular}%
#{latexCaption label}
\\end{table}
|]
