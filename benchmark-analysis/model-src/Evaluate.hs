{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Evaluate
    ( Report(..)
    , RelativeTo(..)
    , SortBy(..)
    , percent
    , evaluateModel
    , compareImplementations
    ) where

import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Function (on)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Monoid (Any(..), (<>))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Numeric (showFFloat)

import Model
import Query
import Schema
import Train

percent :: Real n => n -> n -> Text
percent x y = T.pack $ showFFloat (Just 2) val "%"
  where
    val :: Double
    val = 100 * realToFrac x / realToFrac y

implName :: Implementation -> Text
implName = \case
    Implementation _ _ (Just prettyName) _ _ _ -> prettyName
    Implementation _ name _ _ _ _ -> name

padText :: Int -> Text -> Text
padText n t = t <> T.replicate (n - T.length t) " "

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
  , implTimes :: {-# UNPACK #-} !(Vector Double)
  }

aggregateSteps
    :: MonadIO m => Int -> Model -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps implCount model = snd <$> C.foldl aggregate initial
  where
    initial :: (Int, VariantAggregate)
    initial = (1, VariantAgg
        { variantid = toSqlKey (-1)
        , optimalTime = 0
        , implTimes = VS.replicate implCount 0
        })

    aggregate :: (Int, VariantAggregate) -> StepInfo -> (Int, VariantAggregate)
    aggregate (!lastImpl, !VariantAgg{..}) !StepInfo{..} =
      (newPrediction, VariantAgg
            { variantid = stepVariantId
            , optimalTime = optimalTime + getTime stepBestImpl
            , implTimes = VS.zipWith (+) implTimes $
                    stepTimings `VS.snoc` getTime newPrediction
            })
      where
        newPrediction
          | prediction == -1 = lastImpl
          | otherwise = prediction
          where
            prediction = predict model stepProps

        getTime :: Integral n => n -> Double
        getTime ix = stepTimings `VS.unsafeIndex` (fromIntegral ix - 1)

data TotalStatistics =
  TotalStats
  { variantCount :: {-# UNPACK #-} !Int
  , timesCumRelError :: {-# UNPACK #-} !(Vector Double)
  , relErrorOneToTwo :: {-# UNPACK #-} !(Vector Int)
  , relErrorMoreThanFive :: {-# UNPACK #-} !(Vector Int)
  , relErrorMoreThanTwenty :: {-# UNPACK #-} !(Vector Int)
  , timesMaxRelError :: {-# UNPACK #-} !(Vector Double)
  }

aggregateVariants
    :: MonadIO m
    => IntervalSet Int64
    -> Maybe Int
    -> IntMap Implementation
    -> ConduitT VariantAggregate Void m TotalStatistics
aggregateVariants variantIntervals relTo impls = C.foldM aggregate initial
  where
    initial :: TotalStatistics
    initial = TotalStats
        { variantCount = 0
        , timesCumRelError = zeroDoubleVector
        , relErrorOneToTwo = zeroIntVector
        , relErrorMoreThanFive = zeroIntVector
        , relErrorMoreThanTwenty = zeroIntVector
        , timesMaxRelError = zeroDoubleVector
        }
      where
        zeroIntVector = VS.replicate (IM.size impls) 0
        zeroDoubleVector = VS.replicate (IM.size impls) 0

    getImplName :: Int -> Text
    getImplName ix = implName $ impls IM.! ix

    padSize = 2 + maximum (T.length . implName <$> impls)

    aggregate
        :: MonadIO m
        => TotalStatistics
        -> VariantAggregate
        -> m TotalStatistics
    aggregate TotalStats{..} VariantAgg{..} = liftIO $ do
        when (fromSqlKey variantid `IS.member` variantIntervals) $ do
            T.putStrLn $ "Variant #" <> showText (fromSqlKey variantid)
            T.putStrLn $ relTiming "Optimal" optimalTime
            forM_ ranked $ \(implId, time) -> do
                T.putStrLn $ relTiming (getImplName implId) time
            T.putStrLn ""

        return TotalStats
          { variantCount = variantCount + 1
          , timesCumRelError = VS.zipWith (+) timesCumRelError relTimings

          , relErrorOneToTwo =
                VS.zipWith (lessThan 2) relTimings relErrorOneToTwo

          , relErrorMoreThanFive =
                VS.zipWith (moreThan 5) relTimings relErrorMoreThanFive

          , relErrorMoreThanTwenty =
                VS.zipWith (moreThan 20) relTimings relErrorMoreThanTwenty

          , timesMaxRelError = VS.zipWith max timesMaxRelError relTimings
          }
      where
        lessThan :: Double -> Double -> Int -> Int
        lessThan x val count
            | val < x = count + 1
            | otherwise = count

        moreThan :: Double -> Double -> Int -> Int
        moreThan x val count
            | val > x = count + 1
            | otherwise = count

        relTimings = VS.map (/relToTime) implTimes

        relToTime = case relTo of
            Nothing -> optimalTime
            Just i -> timesCumRelError VS.! i

        relTiming name t = mconcat
            [ "    ", padText padSize (name <> ":"), percent t optimalTime
            , " (" <> showText t <> ")" ]

        ranked = sortBy (comparing snd) . zip [1..] $ VS.toList implTimes

data RelativeTo = Optimal | Predicted | BestNonSwitching
    deriving (Eq,Ord,Show,Read)

data SortBy = Avg | Max
    deriving (Eq,Ord,Show,Read)

data Report = Report
     { reportVariants :: IntervalSet Int64
     , reportRelativeTo :: RelativeTo
     , reportSortBy :: SortBy
     , reportImplTypes :: Set ImplType
     , reportVerbose :: Bool
     }

evaluateModel
    :: Key GPU
    -> Report
    -> Model
    -> TrainingConfig
    -> IntMap Implementation
    -> SqlM ()
evaluateModel gpuId reportConfig@Report{..} model trainConfig oldImpls = do
    stats <- runSqlQuery query $
        foldGroup ((==) `on` stepVariantId) (aggregateSteps newImplCount model)
        .| C.map addBestNonSwitching
        .| aggregateVariants reportVariants relTo impls

    printTotalStatistics reportConfig impls stats
  where
    query = getTotalQuery gpuId trainConfig

    relTo = case reportRelativeTo of
        Optimal -> Nothing
        Predicted -> Just implCount
        BestNonSwitching -> Just $ implCount + 1

    newImplCount = IM.size impls
    implCount = IM.size oldImpls
    impls = oldImpls <> IM.fromList
        [(implCount + 1,
            Implementation (toSqlKey 1) "Predicted" Nothing Nothing Core True)
        ,(implCount + 2,
            Implementation (toSqlKey 1) "Best Non-switching" Nothing Nothing Core True)
        ]

    addBestNonSwitching :: VariantAggregate -> VariantAggregate
    addBestNonSwitching VariantAgg{..} = VariantAgg
        { variantid = variantid
        , optimalTime = optimalTime
        , implTimes = implTimes `VS.snoc` bestNonSwitchingTime
        }
      where
        coreImpls = IM.filter ((==Core) . implementationType) oldImpls

        isCoreImpl :: Int -> Bool
        isCoreImpl = (`IM.member` coreImpls)

        !bestNonSwitchingTime = minimum . map snd $ coreTimes
        coreTimes = filter (isCoreImpl . fst) . zip [1..] $ VS.toList implTimes

compareImplementations
    :: Key GPU -> Report -> IntMap Implementation -> SqlM ()
compareImplementations gpuId reportConfig@Report{..} originalImpls = do

    stats <- runSqlQuery query $
        C.map addBestNonSwitching
        .| aggregateVariants reportVariants relTo impls

    printTotalStatistics reportConfig impls stats
  where
    query = variantInfoQuery gpuId

    relTo = case reportRelativeTo of
        Optimal -> Nothing
        Predicted -> error "Not supported!"
        BestNonSwitching -> Just implCount

    implCount = IM.size originalImpls
    nonSwitching = Implementation (toSqlKey 1) "non-switching" Nothing Nothing Core True

    impls = IM.insert (implCount + 1) nonSwitching originalImpls

    addBestNonSwitching :: VariantInfo -> VariantAggregate
    addBestNonSwitching VariantInfo{..} = VariantAgg
        { variantid = variantId
        , optimalTime = variantOptimal
        , implTimes = variantTimings `VS.snoc` variantBestNonSwitching
        }

printTotalStatistics
    :: MonadIO m => Report -> IntMap Implementation -> TotalStatistics -> m ()
printTotalStatistics Report{..} impls TotalStats{..} = liftIO $ do
    T.putStr . T.unlines $
        [ "Summarising:"
        {-
        , relError "Predicted" predictedCumRelError predictedMaxRelError
        , relError "Best Non-switching"
            bestNonSwitchingCumRelError bestNonSwitchingMaxRelError
            -}
        ]

    forM_ rankedTimings $ relError
  where
    relError (implId, cumError, oneToTwo, gtFive, gtTwenty, maxError) = do
      T.putStr $ padText padSize (getImplName implId <> ":")
      T.putStr . T.pack $ roundedVal (cumError / fromIntegral variantCount)
      when reportVerbose $ do
          T.putStr $ "\t" <> percent oneToTwo variantCount
          T.putStr $ "\t" <> percent gtFive variantCount
          T.putStr $ "\t" <> percent gtTwenty variantCount
      T.putStrLn . T.pack $ "\t" <> roundedVal maxError
      where
        roundedVal val = showFFloat (Just 3) val ""

    rankedTimings = sortBy (comparing compareTime) (VU.toList reportTimings)

    reportTimings :: VU.Vector (Int, Double, Int, Int, Int, Double)
    reportTimings = VU.filter (\(impl,_,_,_,_,_) -> isReportImpl impl) $
      VU.zipWith6 (,,,,,)
                  (VU.generate (VS.length timesCumRelError) (+1))
                  (VS.convert timesCumRelError)
                  (VS.convert relErrorOneToTwo)
                  (VS.convert relErrorMoreThanFive)
                  (VS.convert relErrorMoreThanTwenty)
                  (VS.convert timesMaxRelError)

    getImplName :: Int -> Text
    getImplName ix = implName $ impls IM.! ix

    compareTime :: (Int, Double, Int, Int, Int, Double) -> Double
    compareTime (_, avgTime, _, _, _, maxTime) = case reportSortBy of
        Avg -> avgTime
        Max -> maxTime

    padSize = 2 + maximum (T.length . implName <$> reportImpls)

    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) reportImplTypes

    reportImpls :: IntMap Implementation
    reportImpls = IM.filter (implFilter . implementationType) impls

    isReportImpl :: Int -> Bool
    isReportImpl = (`IM.member` reportImpls)
