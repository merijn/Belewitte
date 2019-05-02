{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Core
    ( Int64
    , Key
    , fromSqlKey
    , toSqlKey
    , MonadIO(liftIO)
    , MonadLogger
    , Log.logErrorN
    , Log.logWarnN
    , Log.logInfoN
    , Log.logDebugN
    , MonadResource
    , MonadThrow(throwM)
    , MonadUnliftIO
    , withUnliftIO
    , UnliftIO(..)
    , ReaderT(..)
    , Text
    , module Core
    ) where

import Control.Exception (Exception(displayException))
import Control.Monad (join, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, handle, throwM)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.IO.Unlift
    (MonadIO(liftIO), MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LoggingT, LogLevel, LogSource, MonadLogger)
import qualified Control.Monad.Logger as Log
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy(Proxy))
import Database.Persist.Sqlite hiding (Connection)
import Database.Sqlite (SqliteException(..))
import Database.Sqlite.Internal
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Foreign (Ptr)
import GHC.Conc.Sync
    (getNumProcessors, setNumCapabilities, setUncaughtExceptionHandler)
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Extras as Lens
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))
import System.IO
    (Handle, IOMode(WriteMode), hPutStrLn, stdout, stderr, withFile)

import Migration
import Schema
import SQLiteExts

data Abort = Abort deriving (Show, Typeable)
instance Exception Abort where

data Error = Error Text deriving (Show)
instance Exception Error

newtype PrettySqliteError = Pretty SqliteException
    deriving (Show, Typeable)

instance Exception PrettySqliteError where
    displayException (Pretty SqliteException{..}) = T.unpack $ mconcat
        [ T.replace "\\\"" "\"" . T.replace "\\n" "\n" $ seFunctionName
        , "\n\n", showText seError, seDetails
        ]

type SqlM = ReaderT (RawSqlite SqlBackend) BaseM

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))
type SqlField rec field = (PersistField field, SqlRecord rec)

data QueryMode = Normal | Explain | ExplainLog FilePath
    deriving (Eq, Read, Show)

newtype BaseM a = BaseM
  { runBaseM :: ReaderT (IORef Text, Maybe Handle)
                        (ResourceT (LoggingT IO))
                        a
  }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger, MonadMask
  , MonadResource, MonadThrow)

instance MonadException BaseM where
    controlIO f = join (withUnliftIO (f . unliftToRunIO))
      where
        unliftToRunIO :: Monad m => UnliftIO m -> RunIO m
        unliftToRunIO (UnliftIO g) = RunIO (fmap return . g)

showText :: Show a => a -> Text
showText = T.pack . show

logIfFail :: Show v => Text -> v -> SqlM a -> SqlM a
logIfFail tag val action = do
    lift . BaseM $ do
        ref <- asks fst
        liftIO . writeIORef ref $ tag <> ": " <> showText val
    action

withTime :: MonadIO m => m a -> m (Double, a)
withTime act = do
    start <- liftIO $ getTime Monotonic
    r <- act
    end <- liftIO $ getTime Monotonic
    let wallTime :: Integer
        wallTime = toNanoSecs (diffTimeSpec start end)
    return $ (fromIntegral wallTime / 1e9, r)

instance MonadFail BaseM where
    fail _ = BaseM $ do
        ref <- asks fst
        msg <- liftIO $ readIORef ref
        Log.logErrorN msg
        throwM Abort

instance MonadUnliftIO BaseM where
  askUnliftIO = BaseM $ withUnliftIO $ \u ->
                            return (UnliftIO (unliftIO u . runBaseM))

data Options a =
  Options
    { database :: Text
    , vacuumDb :: Bool
    , logVerbosity :: LogSource -> LogLevel -> Bool
    , queryMode :: QueryMode
    , migrateSchema :: Bool
    , task :: a
    }

logQueryExplanation :: (Handle -> SqlM ()) -> SqlM ()
logQueryExplanation queryLogger = do
    queryHandle <- lift . BaseM $ asks snd
    case queryHandle of
        Nothing -> return ()
        Just hnd -> queryLogger hnd

runSqlMWithOptions :: Options a -> (a -> SqlM b) -> IO b
runSqlMWithOptions Options{..} work = do
    setUncaughtExceptionHandler (hPutStrLn stderr . displayException)
    handle wrapSqliteException $ do
        getNumProcessors >>= setNumCapabilities
        ref <- newIORef ""
        withQueryLog $ \mHnd -> runStack (ref, mHnd) $ do
            sqlitePtr <- asks $ getSqlitePtr . Lens.view rawSqliteConnection

            registerSqlFunctions sqlitePtr

            didMigrate <- checkMigration migrateSchema

            -- Wait longer before timing out query steps
            rawExecute "PRAGMA busy_timeout = 1000" []

            -- Compacts and reindexes the database when request
            when (vacuumDb || didMigrate) $ rawExecute "VACUUM" []

            workResult <- work task

            -- Runs the ANALYZE command and updates query planner
            rawExecute "PRAGMA optimize" []

            return workResult
  where
    wrapSqliteException :: SqliteException -> IO a
    wrapSqliteException exc = throwM $ Pretty exc

    runStack
        :: (IORef Text, Maybe Handle) -> SqlM a -> IO a
    runStack config =
      runLog . runBase config . withRawSqliteConnInfo connInfo . runReaderT

    withQueryLog :: (Maybe Handle -> IO r) -> IO r
    withQueryLog f = case queryMode of
        Normal -> f Nothing
        Explain -> f (Just stdout)
        ExplainLog p -> withFile p WriteMode $ f . Just

    runLog :: LoggingT IO a -> IO a
    runLog = Log.runStderrLoggingT . Log.filterLogger logVerbosity

    runBase :: (IORef Text, Maybe Handle) -> BaseM a -> LoggingT IO a
    runBase cfg = runResourceT . (`runReaderT` cfg) . runBaseM

    connInfo :: SqliteConnectionInfo
    connInfo = Lens.set fkEnabled True $ mkSqliteConnectionInfo database

    getSqlitePtr :: Connection -> Ptr ()
    getSqlitePtr (Connection _ (Connection' ptr)) = ptr

logThrowM :: (Exception e, MonadLogger m, MonadThrow m) => e -> m r
logThrowM exc = do
    Log.logErrorN . T.pack . displayException $ exc
    throwM exc

showSqlKey :: ToBackendKey SqlBackend record => Key record -> Text
showSqlKey = showText . fromSqlKey

likeFilter :: EntityField record Text -> Text -> Filter record
likeFilter field val =
  Filter field (Left $ T.concat ["%", val, "%"]) (BackendSpecificFilter "like")

whenNotExists
    :: SqlRecord record
    => [Filter record] -> ConduitT i a SqlM () -> ConduitT i a SqlM ()
whenNotExists filters act = lift (selectFirst filters []) >>= \case
    Just _ -> return ()
    Nothing -> act

validateKey
    :: forall record
     . (ToBackendKey SqlBackend record, SqlRecord record)
    => Int64 -> SqlM (Key record)
validateKey n = get key >>= \case
    Just _ -> return key
    Nothing -> do
        let name = entityHaskell $ entityDef (Proxy :: Proxy record)
        Log.logErrorN . mconcat $
            [ "No ", unHaskellName name, " entry found for id: ", showText n ]
        throwM Abort
  where
    key = toSqlKey n

getUniq :: SqlRecord record => record -> SqlM (Key record)
getUniq record = do
    result <- getBy =<< onlyUnique record
    case result of
        Nothing -> insert record
        Just (Entity k _) -> return k

insertUniq
    :: (SqlRecord record, Eq record, Show record)
    => record -> SqlM ()
insertUniq record = do
    result <- insertBy record
    case result of
        Left (Entity _ r) | record /= r -> Log.logErrorN . T.pack $ mconcat
            ["Unique insert failed:\nFound: ", show r, "\nNew: ", show record]
        _ -> return ()

queryImplementations :: Key Algorithm -> SqlM (IntMap Implementation)
queryImplementations algoId = fmap (IM.union builtinImpls) . runConduitRes $
    selectImpls algoId .| C.foldMap toIntMap
  where
    selectImpls aId = selectSource [ ImplementationAlgorithmId ==. aId ] []

    toIntMap :: Entity Implementation -> IntMap Implementation
    toIntMap (Entity k val) = IM.singleton (fromIntegral $ fromSqlKey k) val

    mkImpl :: Text -> Text -> Implementation
    mkImpl short long =
        Implementation algoId short (Just long) Nothing Builtin False

    builtinImpls :: IntMap Implementation
    builtinImpls = IM.fromList
        [ (predictedImplId, mkImpl "predicted" "Predicted")
        , (bestNonSwitchingImplId, mkImpl "best" "Best Non-switching")
        , (optimalImplId, mkImpl "optimal" "Optimal")
        ]
