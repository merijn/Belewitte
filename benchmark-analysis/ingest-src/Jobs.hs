{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jobs (propertyJobs, processProperty, timingJobs, processTiming) where

import Control.Monad (unless, when)
import Control.Monad.Trans (lift)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash.Conduit (hashFile)
import qualified Data.ByteArray (convert)
import Data.Conduit
    (ConduitT, (.|), awaitForever, runConduit, toProducer, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (removeFile)

import Core
import Parsers
import Schema
import Sql (Key, Entity(..), (=.), (==.))
import qualified Sql

(.|>) :: Monad m
      => ConduitT () b m ()
      -> (b -> ConduitT b c m r)
      -> ConduitT a c m ()
producer .|> consumer = toProducer producer .| awaitForever consumer

computeHash :: MonadIO m => FilePath -> m Hash
computeHash path = do
    (digest :: Digest MD5) <- hashFile path
    return . Hash . Data.ByteArray.convert $ digest

propertyJobs
    :: ConduitT () (Text, Key Variant, Key Graph, Maybe Hash, Text) SqlM ()
propertyJobs = Sql.selectSource [] [] .|> toPropJob
  where
    toPropJob
        :: Entity Variant
        -> ConduitT (Entity Variant)
                    (Text, Key Variant, Key Graph, Maybe Hash, Text)
                    SqlM
                    ()
    toPropJob
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
        mkTuple = (showSqlKey varId,varId,graphId,hash,)
        yieldJob = do
            Graph _ _ path _ <- lift $ Sql.getJust graphId
            Algorithm algo _ <- lift $ Sql.getJust algoId
            yield . mkTuple . T.intercalate " " $
                [ showSqlKey varId
                , "-a", algo
                , "-k switch --log"
                , showSqlKey varId <> ".log"
                , fromMaybe "" flags
                , path
                ]

processProperty :: (Key Variant, Key Graph, Maybe Hash, Text) -> SqlM ()
processProperty (varId, graphId, hash, var) = do
    logInfoN $ "Property: " <> var
    liftIO $ removeFile timingFile

    resultHash <- computeHash outputFile
    loadProps <- case hash of
        Nothing -> True <$ Sql.update varId [VariantResult =. Just resultHash]
        Just prevHash | prevHash == resultHash -> return True
        _ -> False <$ logErrorN
                ("Hash mismatch for variant: " <> showSqlKey varId)

    when loadProps $ do
        liftIO $ removeFile outputFile

        runConduit $
            C.sourceFile propLog
            .| C.decode C.utf8
            .| C.map (T.replace "," "")
            .| conduitParse property
            .| C.mapM_ insertProperty

        liftIO $ removeFile propLog
        Sql.update varId [VariantPropsStored =. True]

    logInfoN $ "Property done: " <> var
  where
    propLog :: FilePath
    propLog = T.unpack var <> ".log"

    timingFile :: FilePath
    timingFile = T.unpack var <> ".timings"

    outputFile :: FilePath
    outputFile = T.unpack var <> ".output"

    insertProperty :: Property -> SqlM ()
    insertProperty (GraphProperty name val) =
        Sql.insertUniq $ GraphProp graphId name val

    insertProperty (StepProperty n name val) =
        Sql.insertUniq $ StepProp varId n name val

    insertProperty Prediction{} = return ()

timingJobs
    :: Int -> Key Platform
    -> ConduitT () (Text, Key Variant, Key Implementation, Hash, Text) SqlM ()
timingJobs numRuns platformId = do
    Sql.selectKeys [] [] .|> \algoKey ->
        Sql.selectSource [VariantAlgorithmId ==. algoKey] [] .|> toTimingJob
  where
    toTimingJob
        :: Entity Variant
        -> ConduitT (Entity Variant)
                    (Text, Key Variant, Key Implementation, Hash, Text)
                    SqlM
                    ()
    toTimingJob
        (Entity varId (Variant graphId algoId _ _ Nothing _ _))
        = logErrorN . mconcat $
                [ "Algorithm #", showSqlKey algoId
                , " results missing for graph #", showSqlKey graphId
                , " variant #", showSqlKey varId ]

    toTimingJob
        (Entity varId (Variant graphId algId _ varFlags (Just hash) _ _))
        = Sql.selectSource [ImplementationAlgorithmId ==. algId] [] .|> runImpl
      where
        runImpl (Entity implId (Implementation _ name _ implFlags _ _)) = do
            Sql.whenNotExists (filters ++ [TotalTimerImplId ==. implId]) $ do
                Graph _ _ path _ <- lift $ Sql.getJust graphId
                Algorithm algo _ <- lift $ Sql.getJust algId

                yield . (tag name, varId, implId, hash,) . T.intercalate " " $
                    [ "\"" <> tag name <> "\"", "-a"
                    , algo
                    , fromMaybe ("-k " <> name) implFlags
                    , fromMaybe "" varFlags
                    , "-n " <> showText numRuns
                    , path
                    ]

        filters = [ TotalTimerPlatformId ==. platformId
                  , TotalTimerVariantId ==. varId ]
        tag name = mconcat [ showSqlKey varId, " ", name]

processTiming
    :: Key Platform -> (Key Variant, Key Implementation, Hash, Text) -> SqlM ()
processTiming platformId (varId, implId, hash, var) = do
    logInfoN $ "Timing: " <> var
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
            , " has wrong results for variant #", showSqlKey varId
            , " on Platform #", showSqlKey platformId ]

    runConduit $
        C.sourceFile timingFile
        .| C.decode C.utf8
        .| C.map (T.replace "," "")
        .| conduitParse timer
        .| C.mapM_ (insertTiming timestamp mWrongHash)

    liftIO $ removeFile timingFile

    logInfoN $ "Timing done: " <> var
  where
    timingFile = T.unpack var <> ".timings"
    outputFile = T.unpack var <> ".output"

    insertTiming :: UTCTime -> Maybe Hash -> Timer -> SqlM ()
    insertTiming ts mWrongHash (TotalTiming Timing{..}) = Sql.insert_ $
        TotalTimer platformId varId implId name minTime avgTime maxTime stddev
                   ts mWrongHash

    insertTiming ts mWrongHash (StepTiming n Timing{..})= Sql.insert_ $
        StepTimer platformId varId n implId name minTime avgTime maxTime stddev
                  ts mWrongHash
