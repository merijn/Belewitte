{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module SQLiteExts
    ( registerSqlFunctions
    , wrapSqliteExceptions
    ) where

import Control.Monad ((>=>), when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import Foreign (Ptr, FinalizerPtr, FunPtr, nullPtr, nullFunPtr)
import qualified Foreign
import Foreign.C (CInt(..), CString, withCString)
import Foreign.Storable (Storable)

import Exceptions
import qualified Pretty

data SqliteErrorCode = SqliteErrorCode CInt Text
    deriving (Show, Typeable)

instance Pretty SqliteErrorCode where
    pretty (SqliteErrorCode c_errno msg) = Pretty.vsep
        [ "SQLite function returned error code #" <> pretty errno <> ":"
        , Pretty.reflow msg
        ]
      where
        errno :: Integer
        errno = fromIntegral c_errno

instance Exception SqliteErrorCode where
    toException = toSqlException
    fromException = fromSqlException
    displayException = show . pretty

registerSqlFunctions
    :: (MonadIO m, MonadLogger m, MonadThrow m) => Ptr () -> m ()
registerSqlFunctions sqlitePtr = mapM_ ($sqlitePtr)
    [ createSqlFunction 1 "legacy_random" randomFun
    , createSqlFunction 1 "sqrt" sqliteSqrt
    , createSqlAggregate 3 "init_key_value_vector" infinity
        init_key_value_vector_step key_value_vector_finalise
    , createSqlAggregate 3 "init_key_value_vector_nan" nan
        init_key_value_vector_step key_value_vector_finalise
    , createSqlAggregate_ 4 "update_key_value_vector"
        update_key_value_vector_step key_value_vector_finalise
    , createSqlAggregate_ 1 "check_unique"
        check_unique_step check_unique_finalise
    , createSqlAggregate_ (-1) "min_key"
        min_key_step min_key_finalise
    , createSqlWindow 4 "random_sample" random_sample_step
        random_sample_finalise random_sample_value random_sample_inverse
    , createSqlWindow 1 "count_transitions" count_transitions_step
        count_transitions_finalise count_transitions_value
        count_transitions_inverse
    ]
  where
    infinity :: Double
    infinity = 1/0

    nan :: Double
    nan = 0/0

wrapSqliteExceptions :: (MonadLogger m, MonadCatch m) => m r -> m r
wrapSqliteExceptions = Catch.handle logUnwrappedSqliteException
  where
    logUnwrappedSqliteException exc
        | Just e@SqliteException{} <- fromException exc
        = logThrowM $ PrettySqliteException e

        | otherwise
        = Catch.throwM exc

foreign import ccall "sqlite-functions.h &randomFun"
    randomFun :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &sqlite_sqrt"
    sqliteSqrt :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &init_key_value_vector_step"
    init_key_value_vector_step
        :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &update_key_value_vector_step"
    update_key_value_vector_step
        :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &key_value_vector_finalise"
    key_value_vector_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &check_unique_step"
    check_unique_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &check_unique_finalise"
    check_unique_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &count_transitions_step"
    count_transitions_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &count_transitions_finalise"
    count_transitions_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &count_transitions_value"
    count_transitions_value :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &count_transitions_inverse"
    count_transitions_inverse :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &min_key_step"
    min_key_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &min_key_finalise"
    min_key_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &random_sample_step"
    random_sample_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &random_sample_finalise"
    random_sample_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &random_sample_value"
    random_sample_value :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &random_sample_inverse"
    random_sample_inverse :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import capi "sqlite3.h value SQLITE_ANY"
    sqliteAny :: CInt

foreign import capi "sqlite3.h value SQLITE_OK"
    sqliteOk :: CInt

foreign import ccall "sqlite3.h sqlite3_extended_errcode"
    getExtendedError :: Ptr () -> IO CInt

foreign import ccall "sqlite3.h sqlite3_errstr"
    getErrorString :: CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_create_function_v2"
    createFun :: Ptr () -> CString -> CInt -> CInt -> Ptr ()
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
              -> FunPtr (Ptr () -> IO ())
              -> FunPtr (Ptr () -> IO ())
              -> IO CInt

foreign import ccall "sqlite3.h sqlite3_create_window_function"
    createWindowFun
        :: Ptr () -> CString -> CInt -> CInt -> Ptr ()
        -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
        -> FunPtr (Ptr () -> IO ())
        -> FunPtr (Ptr () -> IO ())
        -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
        -> FunPtr (Ptr () -> IO ())
        -> IO CInt

createSqliteFun
    :: (MonadIO m, MonadLogger m, MonadThrow m, Storable a)
    => CInt
    -> String
    -> Maybe a
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> Ptr ()
    -> m ()
createSqliteFun nArgs strName userData sqlFun aggStep aggFinal sqlPtr = do
    result <- liftIO . withCString strName $ \name -> do
        withUserData $ \(dataPtr, finaliserPtr) ->
            createFun sqlPtr name nArgs sqliteAny dataPtr sqlFun aggStep aggFinal finaliserPtr
    when (result /= sqliteOk) $ do
        bs <- liftIO $ unpackError sqlPtr
        case decodeUtf8' bs of
            Left exc -> logThrowM $ PrettyUnicodeException exc
            Right txt -> logThrowM $ SqliteErrorCode result txt
  where
    unpackError = getExtendedError >=> getErrorString >=> BS.packCString

    allocUserData :: Storable a => a -> IO (Ptr (), FinalizerPtr ())
    allocUserData val = do
        valPtr <- Foreign.new val
        return (Foreign.castPtr valPtr, Foreign.finalizerFree)

    withUserData :: ((Ptr (), FinalizerPtr ()) -> IO b) -> IO b
    withUserData f = case userData of
        Nothing -> f (nullPtr, nullFunPtr)
        Just v -> Catch.bracketOnError (allocUserData v) (Foreign.free . fst) f

noUserData :: Maybe (Ptr ())
noUserData = Nothing

createSqlFunction
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> Ptr ()
    -> m ()
createSqlFunction nArgs name funPtr =
  createSqliteFun nArgs name noUserData funPtr nullFunPtr nullFunPtr

createSqlAggregate
    :: (MonadIO m, MonadLogger m, MonadThrow m, Storable a)
    => CInt
    -> String
    -> a
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> Ptr ()
    -> m ()
createSqlAggregate nArgs name userData =
  createSqliteFun nArgs name (Just userData) nullFunPtr

createSqlAggregate_
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> Ptr ()
    -> m ()
createSqlAggregate_ nArgs name =
  createSqliteFun nArgs name noUserData nullFunPtr

createSqlWindow
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> Ptr ()
    -> m ()
createSqlWindow nArgs strName sqlStep sqlFinal sqlValue sqlInverse sqlPtr = do
    result <- liftIO . withCString strName $ \name -> do
        createWindowFun sqlPtr name nArgs sqliteAny nullPtr sqlStep sqlFinal sqlValue sqlInverse nullFunPtr
    when (result /= sqliteOk) $ do
        bs <- liftIO $ unpackError sqlPtr
        case decodeUtf8' bs of
            Left exc -> logThrowM $ PrettyUnicodeException exc
            Right txt -> logThrowM $ SqliteErrorCode result txt
  where
    unpackError = getExtendedError >=> getErrorString >=> BS.packCString
