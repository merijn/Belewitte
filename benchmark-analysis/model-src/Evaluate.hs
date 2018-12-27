{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Evaluate
    ( Report(..)
    , RelativeTo(..)
    , SortBy(..)
    , percent
    , evaluateModel
    , compareImplementations
    ) where

import Control.Monad (forM_, when)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (fix)
import Data.Bifunctor (second)
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
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as VU
import Numeric (showFFloat)

import Core
import Model
import Query
import Schema
import Train

percent :: Real n => n -> n -> Text
percent x y = T.pack $ showFFloat (Just 2) val "%"
  where
    val :: Double
    val = 100 * realToFrac x / realToFrac y

padText :: Int -> Text -> Text
padText n t = t <> T.replicate (n - T.length t) " "

accumTimings :: (Int64, Double) -> (Int64, Double) -> (Int64, Double)
accumTimings (i1, v1) (i2, v2)
    | i1 == i2 = (i1, v1 + v2)
    | otherwise = error "Shouldn't happen!"

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
  , implTimes :: {-# UNPACK #-} !(Vector (Int64, Double))
  }

aggregateSteps
    :: (MonadFail m, MonadIO m)
    => Model -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps model = do
    Just StepInfo{stepTimings} <- C.peek
    snd <$> C.foldl aggregate (initial stepTimings)
  where
    initial :: Unbox a => Vector (Int64, a) -> (Int, VariantAggregate)
    initial v = (1, VariantAgg
        { variantid = toSqlKey (-1)
        , optimalTime = 0
        , implTimes = VU.map (second (const 0)) v `VU.snoc` (300, 0)
        })

    aggregate :: (Int, VariantAggregate) -> StepInfo -> (Int, VariantAggregate)
    aggregate (!lastImpl, !VariantAgg{..}) !StepInfo{..} =
      (newPrediction, VariantAgg
            { variantid = stepVariantId
            , optimalTime = optimalTime + getTime stepBestImpl
            , implTimes = VU.zipWith accumTimings implTimes $
                    stepTimings `VU.snoc` (300, getTime newPrediction)
            })
      where
        newPrediction
          | prediction == -1 = lastImpl
          | otherwise = prediction
          where
            prediction = predict model stepProps

        getTime :: Integral n => n -> Double
        getTime ix = snd $ stepTimings `VU.unsafeIndex` (fromIntegral ix - 1)

data TotalStatistics =
  TotalStats
  { variantCount :: {-# UNPACK #-} !Int
  , timesCumRelError :: {-# UNPACK #-} !(Vector (Int64, Double))
  , relErrorOneToTwo :: {-# UNPACK #-} !(Vector Int)
  , relErrorMoreThanFive :: {-# UNPACK #-} !(Vector Int)
  , relErrorMoreThanTwenty :: {-# UNPACK #-} !(Vector Int)
  , timesMaxRelError :: {-# UNPACK #-} !(Vector Double)
  }

aggregateVariants
    :: (MonadFail m, MonadIO m)
    => IntervalSet Int64
    -> Maybe Int
    -> IntMap Implementation
    -> ConduitT VariantAggregate Void m TotalStatistics
aggregateVariants variantIntervals relTo impls = do
    Just VariantAgg{implTimes} <- C.peek
    C.foldM aggregate (initial implTimes)
  where
    initial :: Unbox a => Vector (Int64, a) -> TotalStatistics
    initial v = TotalStats
        { variantCount = 0
        , timesCumRelError = VU.map (second (const 0)) v
        , relErrorOneToTwo = zeroIntVector
        , relErrorMoreThanFive = zeroIntVector
        , relErrorMoreThanTwenty = zeroIntVector
        , timesMaxRelError = zeroDoubleVector
        }
      where
        zeroIntVector = VU.map (const 0) v
        zeroDoubleVector = VU.map (const 0) v

    toImplName :: Int64 -> Text
    toImplName ix = getImplName $ impls IM.! fromIntegral ix

    padSize = 2 + maximum (T.length . getImplName <$> impls)

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
                T.putStrLn $ relTiming (toImplName implId) time
            T.putStrLn ""

        return TotalStats
          { variantCount = variantCount + 1
          , timesCumRelError = VU.zipWith accumTimings timesCumRelError relTimings

          , relErrorOneToTwo =
                VU.zipWith (lessThan 2) relTimings relErrorOneToTwo

          , relErrorMoreThanFive =
                VU.zipWith (moreThan 5) relTimings relErrorMoreThanFive

          , relErrorMoreThanTwenty =
                VU.zipWith (moreThan 20) relTimings relErrorMoreThanTwenty

          , timesMaxRelError = VU.zipWith max timesMaxRelError (VU.map snd relTimings)
          }
      where
        lessThan :: Double -> (Int64, Double) -> Int -> Int
        lessThan x (_, val) count
            | val < x = count + 1
            | otherwise = count

        moreThan :: Double -> (Int64, Double) -> Int -> Int
        moreThan x (_, val) count
            | val > x = count + 1
            | otherwise = count

        relTimings = VU.map (second (/relToTime)) implTimes

        relToTime = case relTo of
            Nothing -> optimalTime
            Just i -> snd $ timesCumRelError VU.! i

        relTiming name t = mconcat
            [ "    ", padText padSize (name <> ":"), percent t optimalTime
            , " (" <> showText t <> ")" ]

        ranked = sortBy (comparing snd) $ VU.toList implTimes

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
    :: Key Algorithm
    -> Key GPU
    -> Report
    -> Model
    -> TrainingConfig
    -> IntMap Implementation
    -> SqlM ()
evaluateModel algoId gpuId reportCfg@Report{..} model trainConfig oldImpls = do
    stats <- runSqlQuery query $
        foldGroup ((==) `on` stepVariantId) (aggregateSteps model)
        .| C.map addBestNonSwitching
        .| aggregateVariants reportVariants relTo impls

    printTotalStatistics reportCfg impls stats
  where
    query = getTotalQuery algoId gpuId trainConfig

    relTo = case reportRelativeTo of
        Optimal -> Nothing
        Predicted -> Just implCount
        BestNonSwitching -> Just $ implCount + 1

    implCount = IM.size oldImpls
    impls = oldImpls <> IM.fromList
        [(300,
            Implementation (toSqlKey 1) "Predicted" Nothing Nothing Core True)
        ,(100,
            Implementation (toSqlKey 1) "Best Non-switching" Nothing Nothing Core True)
        ]

    addBestNonSwitching :: VariantAggregate -> VariantAggregate
    addBestNonSwitching VariantAgg{..} = VariantAgg
        { variantid = variantid
        , optimalTime = optimalTime
        , implTimes = implTimes `VU.snoc` (100, bestNonSwitchingTime)
        }
      where
        coreImpls = IM.filter ((==Core) . implementationType) oldImpls

        isCoreImpl :: Int64 -> Bool
        isCoreImpl = (`IM.member` coreImpls) . fromIntegral

        !bestNonSwitchingTime = minimum . map snd $ coreTimes
        coreTimes = filter (isCoreImpl . fst) $ VU.toList implTimes

compareImplementations
    :: Key Algorithm -> Key GPU -> Report -> IntMap Implementation -> SqlM ()
compareImplementations algoId gpuId reportConfig@Report{..} originalImpls = do
    stats <- runSqlQuery query $
        C.map addBestNonSwitching
        .| aggregateVariants reportVariants relTo impls

    printTotalStatistics reportConfig impls stats
  where
    query = variantInfoQuery algoId gpuId

    relTo = case reportRelativeTo of
        Optimal -> Nothing
        Predicted -> error "Not supported!"
        BestNonSwitching -> Just implCount

    implCount = IM.size originalImpls
    nonSwitching = Implementation (toSqlKey 1) "non-switching" Nothing Nothing Core True

    impls = IM.insert 100 nonSwitching originalImpls

    addBestNonSwitching :: VariantInfo -> VariantAggregate
    addBestNonSwitching VariantInfo{..} = VariantAgg
        { variantid = variantId
        , optimalTime = variantOptimal
        , implTimes = variantTimings `VU.snoc` (100, variantBestNonSwitching)
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
      T.putStr $ padText padSize (toImplName implId <> ":")
      T.putStr . T.pack $ roundedVal (cumError / fromIntegral variantCount)
      when reportVerbose $ do
          T.putStr $ "\t" <> percent oneToTwo variantCount
          T.putStr $ "\t" <> percent gtFive variantCount
          T.putStr $ "\t" <> percent gtTwenty variantCount
      T.putStrLn . T.pack $ "\t" <> roundedVal maxError
      where
        roundedVal val = showFFloat (Just 3) val ""

    rankedTimings = sortBy (comparing compareTime) (VU.toList reportTimings)

    reportTimings :: Vector (Int64, Double, Int, Int, Int, Double)
    reportTimings = VU.filter (\(impl,_,_,_,_,_) -> isReportImpl impl) $
      VU.zipWith5 (\(x,y) -> (x,y,,,,))
                  timesCumRelError
                  relErrorOneToTwo
                  relErrorMoreThanFive
                  relErrorMoreThanTwenty
                  timesMaxRelError

    toImplName :: Int64 -> Text
    toImplName ix = getImplName $ impls IM.! fromIntegral ix

    compareTime :: (Int64, Double, Int, Int, Int, Double) -> Double
    compareTime (_, avgTime, _, _, _, maxTime) = case reportSortBy of
        Avg -> avgTime
        Max -> maxTime

    padSize = 2 + maximum (T.length . getImplName <$> reportImpls)

    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) reportImplTypes

    reportImpls :: IntMap Implementation
    reportImpls = IM.filter (implFilter . implementationType) impls

    isReportImpl :: Int64 -> Bool
    isReportImpl = (`IM.member` reportImpls) . fromIntegral
