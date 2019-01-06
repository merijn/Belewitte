{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Monad (when)
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
import Data.Maybe (fromMaybe, listToMaybe)
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

mIf :: Monoid m => Bool -> m -> m
mIf condition v
    | condition = v
    | otherwise = mempty

padText :: Int -> Text -> Text
padText n t = t <> T.replicate (n - T.length t) " "

liftTuple :: (a -> b -> c) -> (Int64, a) -> (Int64, b) -> (Int64, c)
liftTuple f (i1, v1) (i2, v2)
    | i1 == i2 = (i1, f v1 v2)
    | otherwise = error $ mconcat
        [ "Shouldn't happen! Found: " , show i1, " and ", show i2]

reportImplementations
    :: Int -> IntMap Implementation -> (a -> Text) -> [(Int64, a)] -> IO ()
reportImplementations padding impls f =
    mapM_ $ \(implId, val) -> T.putStrLn $ mconcat
        [ T.replicate padding " "
        , padText padSize $ toImplName implId <> ":"
        , f val
        ]
  where
    toImplName :: Int64 -> Text
    toImplName ix = getImplName $ impls IM.! fromIntegral ix

    padSize :: Int
    padSize = 2 + maximum (T.length . getImplName <$> impls)

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
    => Int -> Model -> ConduitT StepInfo Void m VariantAggregate
aggregateSteps defaultImpl model = do
    Just StepInfo{stepVariantId,stepTimings} <- C.peek

    let zeroTimeVec :: Vector (Int64, Double)
        zeroTimeVec = VU.map (second (const 0)) stepTimings `VU.snoc`
            (predictedImplId, 0)

        initial :: (Int, VariantAggregate)
        initial = (defaultImpl, VariantAgg
            { variantid = stepVariantId
            , optimalTime = 0
            , implTimes = zeroTimeVec
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
            , implTimes = VU.zipWith (liftTuple (+)) implTimes $
                    stepTimings `VU.snoc` newPrediction
            })
      where
        newPrediction :: (Int64, Double)
        newPrediction = (predictedImplId, getTime predictedImpl)

        predictedImpl :: Int
        predictedImpl
          | prediction == -1 = lastImpl
          | otherwise = prediction
          where
            prediction = predict model stepProps

        getTime :: Integral n => n -> Double
        getTime ix = snd $
            stepTimings `VU.unsafeIndex` (translateMap IM.! fromIntegral ix)

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
    -> RelativeTo
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

    aggregate
        :: MonadIO m
        => TotalStatistics
        -> VariantAggregate
        -> m TotalStatistics
    aggregate TotalStats{..} VariantAgg{..} = liftIO $ do
        when (fromSqlKey variantid `IS.member` variantIntervals) $ do
            T.putStrLn $ "Variant #" <> showText (fromSqlKey variantid)
            reportImplementations 4 impls relTiming ranked
            T.putStrLn ""

        return TotalStats
          { variantCount = variantCount + 1
          , timesCumRelError =
                VU.zipWith (liftTuple (+)) timesCumRelError relTimings

          , relErrorOneToTwo =
                VU.zipWith (lessThan 2) relTimings relErrorOneToTwo

          , relErrorMoreThanFive =
                VU.zipWith (moreThan 5) relTimings relErrorMoreThanFive

          , relErrorMoreThanTwenty =
                VU.zipWith (moreThan 20) relTimings relErrorMoreThanTwenty

          , timesMaxRelError =
                VU.zipWith max timesMaxRelError . VU.map snd $ relTimings
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

        findImplTime :: Int64 -> Maybe Double
        findImplTime i = snd <$> VU.find ((i==) . fst) timesCumRelError

        relToTime :: Double
        relToTime = fromMaybe (error "Implementation not found!") $
            case relTo of
                Optimal -> Just optimalTime
                Predicted -> findImplTime predictedImplId
                BestNonSwitching -> findImplTime bestNonSwitchingImplId

        relTiming t = percent t optimalTime <> " (" <> showText t <> ")"

        rankImpls :: Vector (Int64, Double) -> [(Int64, Double)]
        rankImpls = sortBy (comparing snd) . VU.toList

        ranked :: [(Int64, Double)]
        ranked = (optimalImplId, optimalTime) : rankImpls implTimes

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
    -> Key Platform
    -> Either Int Text
    -> Report
    -> Model
    -> TrainingConfig
    -> SqlM ()
evaluateModel algoId platId defImpl reportCfg@Report{..} model trainConfig = do
    impls <- queryImplementations algoId
    let lookupByName :: Text -> Maybe Int
        lookupByName t = fmap fst
                       . listToMaybe
                       . filter ((t==) . implementationName . snd)
                       $ IM.toList impls

    defaultImpl <- case defImpl of
        Left i | IM.member i impls -> return i
        Right t | Just i <- lookupByName t -> return i
        _ -> logThrowM $ Error "Default implementation not found!"

    stats <- runSqlQuery query $
        foldGroup ((==) `on` stepVariantId) (aggregateSteps defaultImpl model)
        .| C.map (addBestNonSwitching impls)
        .| aggregateVariants reportVariants reportRelativeTo impls

    printTotalStatistics reportCfg impls stats
  where
    query = getTotalQuery algoId platId trainConfig

    addBestNonSwitching
        :: IntMap Implementation -> VariantAggregate -> VariantAggregate
    addBestNonSwitching impls VariantAgg{..} = VariantAgg
        { variantid = variantid
        , optimalTime = optimalTime
        , implTimes = implTimes `VU.snoc`
            (bestNonSwitchingImplId, bestNonSwitchingTime)
        }
      where
        isCoreImpl :: Int64 -> Bool
        isCoreImpl i = case IM.lookup (fromIntegral i) impls of
            Just Implementation{implementationType = Core} -> True
            _ -> False

        !bestNonSwitchingTime =
          VU.minimum . VU.map snd . VU.filter (isCoreImpl . fst) $ implTimes

compareImplementations
    :: Key Algorithm -> Key Platform -> Report -> SqlM ()
compareImplementations algoId platformId reportConfig@Report{..} = do
    impls <- queryImplementations algoId
    stats <- runSqlQuery query $
        C.map addBestNonSwitching
        .| aggregateVariants reportVariants reportRelativeTo impls

    printTotalStatistics reportConfig impls stats
  where
    query = variantInfoQuery algoId platformId

    addBestNonSwitching :: VariantInfo -> VariantAggregate
    addBestNonSwitching VariantInfo{..} = VariantAgg
        { variantid = variantId
        , optimalTime = variantOptimal
        , implTimes = variantTimings `VU.snoc`
            (bestNonSwitchingImplId, variantBestNonSwitching)
        }

printTotalStatistics
    :: MonadIO m => Report -> IntMap Implementation -> TotalStatistics -> m ()
printTotalStatistics Report{..} impls TotalStats{..} = liftIO $ do
    T.putStrLn "Summarising:"
    reportImplementations 0 impls relError rankedTimings
  where
    relError (cumError, oneToTwo, gtFive, gtTwenty, maxError) = mconcat
        [ T.pack $ roundedVal (cumError / fromIntegral variantCount)
        , mIf reportVerbose $ mconcat
                [ "\t" <> percent oneToTwo variantCount
                , "\t" <> percent gtFive variantCount
                , "\t" <> percent gtTwenty variantCount
                ]
        , T.pack $ "\t" <> roundedVal maxError
        ]
      where
        roundedVal val = showFFloat (Just 3) val ""

    rankedTimings = sortBy (comparing compareTime) $ VU.toList reportTimings

    reportTimings :: Vector (Int64, (Double, Int, Int, Int, Double))
    reportTimings = VU.filter (\(impl,_) -> isReportImpl impl) $
      VU.zipWith5 (\(i,a) b c d e-> (i,(a,b,c,d,e)))
                  timesCumRelError
                  relErrorOneToTwo
                  relErrorMoreThanFive
                  relErrorMoreThanTwenty
                  timesMaxRelError

    compareTime :: (Int64, (Double, Int, Int, Int, Double)) -> Double
    compareTime (_, (avgTime, _, _, _, maxTime)) = case reportSortBy of
        Avg -> avgTime
        Max -> maxTime

    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) reportImplTypes

    reportImpls :: IntMap Implementation
    reportImpls = IM.filter (implFilter . implementationType) impls

    isReportImpl :: Int64 -> Bool
    isReportImpl = (`IM.member` reportImpls) . fromIntegral
