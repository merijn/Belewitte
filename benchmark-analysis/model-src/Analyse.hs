{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Analyse
    ( Report(..)
    , RelativeTo(..)
    , SortBy(..)
    , percent
    , analyseModel
    ) where

import Control.Applicative (ZipList(..))
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
import Data.Monoid (Any(..))
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Numeric (showFFloat)

import Model
import Query
import Schema

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

data StepAggregate =
  StepAgg
  { variantid :: {-# UNPACK #-} !(Key Variant)
  , optimalTime :: {-# UNPACK #-} !Double
  , predictedTime :: {-# UNPACK #-} !Double
  , implTimes :: {-# UNPACK #-} !(Vector Double)
  , lastImpl :: {-# UNPACK #-} !Int
  }

aggregateSteps
    :: MonadIO m
    => IntMap Implementation
    -> Model
    -> ConduitT StepInfo Void m StepAggregate
aggregateSteps impls model = C.foldM aggregate initial
  where
    initial :: StepAggregate
    initial = StepAgg
        { variantid = toSqlKey (-1)
        , optimalTime = 0
        , predictedTime = 0
        , implTimes = VS.replicate (IM.size impls) 0
        , lastImpl = 1
        }

    aggregate :: MonadIO m => StepAggregate -> StepInfo -> m StepAggregate
    aggregate !StepAgg{..} !StepInfo{..} =
        return StepAgg
            { variantid = variantId
            , optimalTime = optimalTime + getTime bestImpl
            , predictedTime = predictedTime + getTime newPrediction
            , implTimes = VS.zipWith (+) implTimes timings
            , lastImpl = newPrediction
            }
      where
        newPrediction
          | prediction == -1 = lastImpl
          | otherwise = prediction
          where
            prediction = predict model props

        getTime :: Integral n => n -> Double
        getTime ix = timings `VS.unsafeIndex` (fromIntegral ix - 1)

data VariantAggregate =
  VariantAgg
  { variantCount :: {-# UNPACK #-} !Int
  , predictedCumRelError :: {-# UNPACK #-} !Double
  , oracleCumRelError :: {-# UNPACK #-} !Double
  , timesCumRelError :: {-# UNPACK #-} !(Vector Double)
  , predictedMaxRelError :: {-# UNPACK #-} !Double
  , oracleMaxRelError :: {-# UNPACK #-} !Double
  , timesMaxRelError :: {-# UNPACK #-} !(Vector Double)
  }

aggregateVariants
    :: MonadIO m
    => IntervalSet Int64
    -> RelativeTo
    -> IntMap Implementation
    -> ConduitT StepAggregate Void m VariantAggregate
aggregateVariants variantIntervals relTo impls = C.foldM aggregate initial
  where
    initial :: VariantAggregate
    initial = VariantAgg
        { variantCount = 0
        , predictedCumRelError = 0
        , oracleCumRelError = 0
        , timesCumRelError = zeroVector
        , predictedMaxRelError = 0
        , oracleMaxRelError = 0
        , timesMaxRelError = zeroVector
        }
      where
        zeroVector = VS.replicate (IM.size impls) 0

    coreImpls = IM.filter ((==Core) . implementationType) impls

    isCoreImpl :: Int -> Bool
    isCoreImpl = (`IM.member` coreImpls)

    getImplName :: Int -> Text
    getImplName ix = implName $ impls IM.! ix

    padSize = 2 + maximum (T.length . implName <$> impls)

    aggregate
        :: MonadIO m => VariantAggregate -> StepAggregate -> m VariantAggregate
    aggregate VariantAgg{..} StepAgg{..} = liftIO $ do
        when (fromSqlKey variantid `IS.member` variantIntervals) $ do
            T.putStrLn $ "Variant #" <> showText (fromSqlKey variantid)
            T.putStrLn $ relTiming "Optimal" optimalTime
            T.putStrLn $ relTiming "Predicted" predictedTime
            forM_ ranked $ \(implId, time) -> do
                T.putStrLn $ relTiming (getImplName implId) time
            T.putStrLn ""

        return VariantAgg
            { variantCount = variantCount + 1
            , predictedCumRelError =
                    predictedCumRelError + (predictedTime / relToTime)

            , oracleCumRelError =
                    oracleCumRelError + (oracleTime / relToTime)

            , timesCumRelError = VS.zipWith (+) timesCumRelError $
                    VS.map (/relToTime) implTimes

            , predictedMaxRelError =
                    max predictedMaxRelError (predictedTime / relToTime)

            , oracleMaxRelError =
                    max oracleMaxRelError (oracleTime / relToTime)

            , timesMaxRelError = VS.zipWith max timesMaxRelError $
                    VS.map (/relToTime) implTimes
            }
      where
        relToTime = case relTo of
            Optimal -> optimalTime
            Predicted -> predictedTime
            Oracle -> oracleTime

        !oracleTime = minimum . map snd $ coreTimes
        coreTimes = filter (isCoreImpl . fst) . zip [1..] $ VS.toList implTimes

        relTiming name t = mconcat
            [ "    ", padText padSize (name <> ":"), percent t optimalTime
            , " (" <> showText t <> ")" ]

        ranked = sortBy (comparing snd) . zip [1..] $ VS.toList implTimes

data RelativeTo = Optimal | Predicted | Oracle
    deriving (Eq,Ord,Show,Read)

data SortBy = Avg | Max
    deriving (Eq,Ord,Show,Read)

data Report = Report
     { reportVariants :: IntervalSet Int64
     , reportRelativeTo :: RelativeTo
     , reportSortBy :: SortBy
     , reportImplTypes :: Set ImplType
     }

analyseModel
    :: Report -> Model -> Query StepInfo -> IntMap Implementation -> SqlM ()
analyseModel Report{..} model query impls = do
    VariantAgg{..} <- runSqlQuery query $
        foldGroup ((==) `on` variantId) (aggregateSteps impls model)
        .| aggregateVariants reportVariants reportRelativeTo impls

    let relError name val maxErr = mconcat
            [ padText padSize (name <> ":")
            , T.pack $
                showFFloat (Just 3) (val / fromIntegral variantCount) "    ("
            , T.pack $
                showFFloat (Just 3) maxErr ")"
            ]

        rankedTimings = sortBy (comparing compareTime) reportTimings
        reportTimings = filter (\(impl,_,_) -> isReportImpl impl) implTimings
        implTimings = getZipList $
            (,,) <$> ZipList [1..]
                 <*> ZipList (VS.toList timesCumRelError)
                 <*> ZipList (VS.toList timesMaxRelError)

    liftIO $ do
        T.putStr . T.unlines $
            [ "Summarising:"
            , relError "Predicted" predictedCumRelError predictedMaxRelError
            , relError "Oracle" oracleCumRelError oracleMaxRelError
            ]

        forM_ rankedTimings $ \(implId, avgTime, maxTime) -> do
            T.putStrLn $ relError (getImplName implId) avgTime maxTime
  where
    getImplName :: Int -> Text
    getImplName ix = implName $ impls IM.! ix

    padSize = 2 + maximum (T.length . implName <$> impls)

    implFilter :: ImplType -> Bool
    implFilter = getAny . foldMap (\i -> Any . (==i)) reportImplTypes

    reportImpls :: IntMap Implementation
    reportImpls = IM.filter (implFilter . implementationType) impls

    isReportImpl :: Int -> Bool
    isReportImpl = (`IM.member` reportImpls)

    compareTime :: (Int, Double, Double) -> Double
    compareTime (_, avgTime, maxTime) = case reportSortBy of
        Avg -> avgTime
        Max -> maxTime
