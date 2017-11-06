{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Concurrent hiding (yield)
import Control.Monad.Catch (Exception, mask_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger
    ( LogSource, LogLevel(..), filterLogger, runChanLoggingT
    , runStderrLoggingT, unChanLoggingT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource
import Data.Acquire (allocateAcquire)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Process
    (CreateProcess, Inherited(..), proc, withCheckedProcessCleanup)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import Database.Persist.Sqlite
import GHC.Conc.Sync (getNumProcessors)
import Numeric (showFFloat)
import System.IO (hClose)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Types (Fd)

import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable     as V

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

import Data.Int (Int32, Int64)
import Data.Binary.Get
import Data.Binary.Put

--import BroadcastChan
import Schema

type Props = [Double]
type Algorithms = Int64

data TreeNode
    = Node
      { threshold :: {-# UNPACK #-} !Double
      , propIdx :: {-# UNPACK #-} !Int
      , leftNode :: {-# UNPACK #-} !Int
      , rightNode :: {-# UNPACK #-} !Int
      } deriving (Show)

instance Storable TreeNode where
    sizeOf _ = sizeOf (undefined :: Double) + 4 * sizeOf (undefined :: Int32)
    alignment _ = alignment (undefined :: Double)
    peek ptr = Node <$> peek (castPtr ptr)
                    <*> (fromIntegral <$> peekInt32 0)
                    <*> (fromIntegral <$> peekInt32 1)
                    <*> (fromIntegral <$> peekInt32 2)

      where
        peekInt32 :: Int -> IO Int32
        peekInt32 n = peekByteOff ptr (sizeOf (undefined :: Double) + n * sizeOf (undefined :: Int32))

    poke ptr Node{..} = do
        poke (castPtr ptr) threshold
        pokeInt32 0 (fromIntegral propIdx)
        pokeInt32 1 (fromIntegral leftNode)
        pokeInt32 2 (fromIntegral rightNode)
      where
        pokeInt32 :: Int -> Int32 -> IO ()
        pokeInt32 n val = pokeByteOff ptr (sizeOf (undefined :: Double) + n * sizeOf (undefined :: Int32)) val

predict :: V.Vector TreeNode -> V.Vector Double -> Int
predict tree props = go (tree `V.unsafeIndex` 0)
  where
    go :: TreeNode -> Int
    go !Node{..}
        | leftNode == -1 = rightNode
        | props `V.unsafeIndex` propIdx <= threshold = go (tree `V.unsafeIndex` leftNode)
        | otherwise = go (tree `V.unsafeIndex` rightNode)

sizeOfElem :: (Storable a) => V.Vector a -> Int
sizeOfElem vec = sizeOf (undefined `asTypeOf` V.head vec)

byteStringToVector :: (Storable a) => BS.ByteString -> V.Vector a
byteStringToVector bs = vec where
    vec = V.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = BS.toForeignPtr bs
    scale = (`div` sizeOfElem vec)

data Query =
  Query
    { params :: [PersistValue]
    , query :: Bool -> Text
    }

runSqlQuery
    :: (Exception e)
    => Query
    -> ([PersistValue] -> Either e r)
    -> Source SqlM r
runSqlQuery Query{params,query} convert = do
    srcRes <- lift . runSql $ rawQueryRes (query True) params
    (_, src) <- allocateAcquire srcRes
    src .| converter
    where
      converter = awaitForever $ \row -> do
              case convert row of
                  Left err -> throwM err
                  Right r -> yield r

runSqlQueryCount :: Query -> SqlM Int
runSqlQueryCount Query{params,query} = runSql $ do
    result <- withRawQuery (query False) params await
    case result of
        Just [PersistInt64 n] -> return $ fromIntegral n
        _ -> throwM $ Error "Error computing count!"

data Error = Error Text deriving (Show)
instance Exception Error

putProps :: Props -> ByteString
putProps = LBS.toStrict . runPut . mapM_ putDoublehost

putResults :: Algorithms -> ByteString
putResults = LBS.toStrict . runPut . putInt64host

propertyQuery
    :: Key GPU
    -> Set Text
    -> Set Text
    -> SqlM (Int, Int, Source SqlM (Props, Algorithms))
propertyQuery gpuId graphProps stepProps = do
    numEntries <- runSqlQueryCount query
    return (numProps, numEntries, runSqlQuery query unwrapProps)
  where
    numProps :: Int
    numProps = S.size graphProps + S.size stepProps

    unwrapProps :: [PersistValue] -> Either Error (Props, Algorithms)
    unwrapProps = go id numProps
      where
        go f 0 [PersistInt64 n] = Right (f [], n)
        go f n (PersistDouble d:vs) = go (f . (d:)) (n-1) vs
        go _ _ _ = Left . Error $ "Unexpect value!"

    query :: Query
    query = Query [] $ \b -> [i|
SELECT #{if b then genClauses select <> "Impl.implId" else "COUNT(*)"}
FROM Variant
     INNER JOIN Graph ON Variant.graphId = Graph.id
     INNER JOIN StepProp AS Step ON Variant.id = Step.variantId
#{genClauses joinClause}
     INNER JOIN
     ( SELECT variantId, stepId, implId
       FROM StepTime
       WHERE gpuId = #{fromSqlKey gpuId}
       GROUP BY variantId, stepId
       HAVING avgRuntime = MIN(avgRuntime)) AS Impl
     ON Variant.id = Impl.variantId AND Step.stepId = Impl.stepId
WHERE #{mconcat . intersperse " AND " $ genClauses whereClause}
ORDER BY Graph.name, Variant.variant, Step.stepId ASC|]
      where
        genClauses :: Monoid m => (Text -> (Word, Text) -> m) -> m
        genClauses f =
            clause "GraphProp" graphProps <> clause "StepProp" stepProps
          where
            clause table = foldMap (f table) . zip [0..] . S.toList

        select :: Text -> (Word, a) -> Text
        select table (n, _) = [i|#{table}#{n}.value, |]

        joinClause :: Text -> (Word, a) -> Text
        joinClause tbl (n, _) = case tbl of
            "GraphProp" -> clause [i|Variant.graphId = #{tbl}#{n}.graphId|]
            "StepProp" -> clause $ [i|Variant.id = #{tbl}#{n}.variantId|]
                                <> [i| AND #{tbl}#{n}.stepId = Step.stepId|]
            _ -> ""
          where
            clause s =
              [i|     INNER JOIN #{tbl} AS #{tbl}#{n} ON |] <> s <> "\n"

        whereClause :: Text -> (Word, Text) -> [Text]
        whereClause table (n, prop) = [[i|#{table}#{n}.property = '#{prop}'|]]

trainModel :: (Int, Int, Source SqlM (Props, Algorithms)) -> SqlM ByteString
trainModel (numProps, numEntries, propSrc) = do
    ((outFd, closeOut), (resultsHnd, closeResults)) <- mask_ $ do
        (fdOut, fdIn) <- liftIO createPipe
        hndIn <- liftIO $ fdToHandle fdIn
        closeIn <- register $ hClose hndIn
        closeOut <- register $ closeFd fdOut
        return ((fdOut, closeOut), (hndIn, closeIn))

    let handleStreams (propSink, closeSink) hnd Inherited = do
            register $ hClose hnd
            release closeOut
            processIn <- register closeSink

            let propertySink = ZipSink $ do
                    C.map (putProps . fst) .| propSink
                    release processIn

                resultSink = ZipSink $ do
                    C.map (putResults . snd) .| C.sinkHandle resultsHnd
                    release closeResults

                combinedSink = getZipSink (propertySink <* resultSink)

            runConduit $ propSrc .| combinedSink
            liftIO $ BS.hGetContents hnd

    withCheckedProcessCleanup (process outFd) handleStreams
  where
    process :: Fd -> CreateProcess
    process fd = proc "./model.py"
        [ "--entries"
        , show numEntries
        , "--properties"
        , show numProps
        , "--fd"
        , show fd
        ]

parseEntry :: Get (Double, Int32, Int32, Int32)
parseEntry = (,,,) <$> getDoublehost <*> getInt32host <*> getInt32host <*> getInt32host <* getInt32host

runParse :: Get a -> ByteString -> a
runParse parser bs = x
  where
    Done _ _ x = pushEndOfInput $ pushChunk (runGetIncremental parser) bs

main :: IO ()
main = do
    numCPUs <- getNumProcessors
    setNumCapabilities numCPUs

    logChan <- newChan
    forkIO . runStderrLoggingT . filterLogger isNotDebug $
        unChanLoggingT logChan

    runChanLoggingT logChan . runWithDb "test.db" numCPUs $ do
        runSql $ runMigrationSilent migrateAll
        graphProps <- gatherProps "GraphProp"
        stepProps <- gatherProps "StepProp"
        input <- trainModel =<< propertyQuery (toSqlKey 1) graphProps stepProps
        let predictor = predict (byteStringToVector input) . V.fromList
            aggregate (!right,!wrong) (x,y)
                | fromIntegral x == y = (right + 1, wrong)
                | otherwise = (right, wrong + 1)

        (_, _, propSrc) <- propertyQuery (toSqlKey 1) graphProps stepProps

        (right, wrong) :: (Int, Int) <- runConduit $
            propSrc .| C.map (first predictor) .| C.foldl aggregate (0,0)

        liftIO $ do
            putStrLn $ "Right predictions: " ++ show right
            putStrLn $ "Wrong predictions: " ++ show wrong
            putStrLn $ "Error rate: " ++ percent wrong (right+wrong)

  where
    percent :: Integral n => n -> n -> String
    percent x y = showFFloat (Just 2) val "%"
      where
        val :: Double
        val = 100 * fromIntegral x / fromIntegral y

    gatherProps :: Text -> SqlM (Set Text)
    gatherProps table =
        runSql . runConduit $ rawQuery query [] .| C.foldMap toSet
      where
        toSet [PersistText t] = S.singleton t
        toSet _ = S.empty

        query = [i|SELECT DISTINCT property FROM #{table}|]

    isNotDebug :: LogSource -> LogLevel -> Bool
    isNotDebug _ LevelDebug = False
    isNotDebug _ _ = True

    runWithDb :: Text -> Int -> SqlM a -> LoggingT IO a
    runWithDb path n = runResourceT . withSqlitePool path n . runReaderT
