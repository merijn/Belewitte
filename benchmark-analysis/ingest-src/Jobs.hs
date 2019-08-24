{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jobs
    ( (.>)
    , variantToPropertyJob
    , processProperty
    , variantToTimingJob
    , filterExistingTimings
    , processTiming
    ) where

import Control.Monad (unless, when)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit (ConduitT, (.|), awaitForever, runConduit, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (removeFile)

import Core
import Parsers
import ProcessPool (Job(..), Result(..))
import Schema
import Sql (Entity(..), Key, MonadSql, (=.), (==.))
import qualified Sql

(.>) :: Monad m
      => ConduitT a b m ()
      -> (b -> ConduitT b c m r)
      -> ConduitT a c m ()
producer .> consumer = producer .| awaitForever consumer
infixl 3 .>

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: Digest MD5) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

variantToPropertyJob
    :: Entity Variant
    -> ConduitT (Entity Variant)
                (Job (Key Graph, Maybe Hash))
                SqlM
                ()
variantToPropertyJob
    (Entity varId (Variant graphId algoId _ flags hash hasProps retries)) =
        case hash of
            Nothing
                | retries < 5 -> yieldJob
                | otherwise -> logWarnN $
                    "Hash missing, but too many retries for variant #"
                    <> showSqlKey varId

            Just _
                | not hasProps && retries < 5 -> yieldJob
                | not hasProps -> logWarnN $
                    "Too many retries for variant #" <> showSqlKey varId
                | otherwise -> return ()
  where
    yieldJob = do
        Graph _ path _ _ <- Sql.getJust graphId
        Algorithm algo _ <- Sql.getJust algoId
        yield . Job (graphId,hash) varId (showSqlKey varId) . T.unwords $
            [ showSqlKey varId
            , "-a", algo
            , "-k switch --log"
            , showSqlKey varId <> ".log"
            , fromMaybe "" flags
            , path
            ]

processProperty :: Result (Key Graph, Maybe Hash) -> SqlM ()
processProperty Result{resultValue=(graphId, hash), ..} = do
    logInfoN $ "Property: " <> resultLabel
    liftIO $ removeFile timingFile

    resultHash <- computeHash outputFile
    loadProps <- case hash of
        Nothing ->
            True <$ Sql.update resultVariant [VariantResult =. Just resultHash]

        Just prevHash | prevHash == resultHash -> return True
        _ -> False <$ logErrorN
                ("Hash mismatch for variant: " <> showSqlKey resultVariant)

    when loadProps $ do
        liftIO $ removeFile outputFile

        runConduit $
            C.sourceFile propLog
            .| C.decode C.utf8
            .| C.map (T.replace "," "")
            .| conduitParse property
            .| C.mapM_ insertProperty

        liftIO $ removeFile propLog
        Sql.update resultVariant [VariantPropsStored =. True]

    logInfoN $ "Property done: " <> resultLabel
  where
    propLog :: FilePath
    propLog = T.unpack resultLabel <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack resultLabel <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack resultLabel <> ".output"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        Sql.insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        Sql.insertUniq $ StepProp resultVariant n name val

    insertProperty Prediction{} = return ()

variantToTimingJob
    :: (MonadLogger m, MonadResource m, MonadSql m)
    => Int
    -> Entity Variant
    -> ConduitT (Entity Variant) (Job (Key Implementation, Hash)) m ()
variantToTimingJob _ (Entity varId (Variant graphId algoId _ _ Nothing _ _))
    = logErrorN . mconcat $
        [ "Algorithm #", showSqlKey algoId
        , " results missing for graph #", showSqlKey graphId
        , " variant #", showSqlKey varId
        ]

variantToTimingJob runs (Entity varId Variant{variantResult = Just hash,..}) =
    Sql.selectSource [ImplementationAlgorithmId ==. variantAlgorithmId] []
    .| C.mapM toMissingJob
  where
    tag name = mconcat [ showSqlKey varId, " ", name]

    toMissingJob
        :: MonadSql m
        => Entity Implementation -> m (Job (Key Implementation, Hash))
    toMissingJob (Entity implId Implementation{..}) = do
        Graph _ path _ _ <- Sql.getJust variantGraphId
        Algorithm algo _ <- Sql.getJust variantAlgorithmId

        return . Job (implId, hash) varId (tag implementationName) $ T.unwords
            [ "\"" <> tag implementationName <> "\"", "-a"
            , algo
            , fromMaybe ("-k " <> implementationName) implementationFlags
            , fromMaybe "" variantFlags
            , "-n " <> showText runs
            , path
            ]

filterExistingTimings
    :: (MonadLogger m, MonadResource m, MonadSql m)
    => Key Platform
    -> (Job (Key Implementation, Hash))
    -> ConduitT (Job (Key Implementation, Hash))
                (Job (Key Implementation, Hash))
                m
                ()
filterExistingTimings platformId job@Job{jobVariant, jobValue = (implId, _)} =
    Sql.whenNotExists filters $ yield job
  where
    filters = [ TotalTimerPlatformId ==. platformId
              , TotalTimerVariantId ==. jobVariant
              , TotalTimerImplId ==. implId
              ]

processTiming
    :: (MonadLogger m, MonadResource m, MonadSql m, MonadThrow m)
    => Key Platform -> Result (Key Implementation, Hash) -> m ()
processTiming platformId Result{resultValue = (implId, hash), ..} = do
    logInfoN $ "Timing: " <> resultLabel
    timestamp <- liftIO getCurrentTime
    resultHash <- computeHash outputFile
    liftIO $ removeFile outputFile

    let correct = resultHash == hash
        mWrongHash
            | correct = Nothing
            | otherwise = Just resultHash

    unless correct $ do
        logErrorN . mconcat $
            [ "Implementation #", showSqlKey implId
            , " has wrong results for variant #", showSqlKey resultVariant
            , " on Platform #", showSqlKey platformId ]

    runConduit $
        C.sourceFile timingFile
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse timer
        .| C.mapM_ (insertTiming timestamp mWrongHash)

    liftIO $ removeFile timingFile

    logInfoN $ "Timing done: " <> resultLabel
  where
    timingFile = T.unpack resultLabel <> ".timings"
    outputFile = T.unpack resultLabel <> ".output"

    insertTiming :: MonadSql m => UTCTime -> Maybe Hash -> Timer -> m ()
    insertTiming ts mWrongHash (TotalTiming Timing{..}) = Sql.insert_ $
        TotalTimer platformId resultVariant implId name minTime avgTime maxTime
                   stddev ts mWrongHash

    insertTiming ts mWrongHash (StepTiming n Timing{..})= Sql.insert_ $
        StepTimer platformId resultVariant n implId name minTime avgTime
                  maxTime stddev ts mWrongHash
