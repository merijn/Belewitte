{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
module SQLiteExts
    ( initialiseSqlite
    , registerSqlFunctions
    , wrapSqliteExceptions
    ) where

import Control.Monad ((>=>), when)
import Control.Monad.Catch (MonadCatch, MonadThrow, handle, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import Database.Sqlite (SqliteException(..))
import Foreign (Ptr, FunPtr, castFunPtr, nullPtr, nullFunPtr)
import Foreign.C (CInt(..), CString, withCString)

import Exceptions
import Pretty (Pretty(pretty))
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
    , createSqlAggregate 3 "double_vector"
        double_vector_step double_vector_finalise
    , createSqlAggregate 3 "init_key_value_vector"
        init_key_value_vector_step key_value_vector_finalise
    , createSqlAggregate 4 "update_key_value_vector"
        update_key_value_vector_step key_value_vector_finalise
    , createSqlAggregate 1 "check_unique"
        check_unique_step check_unique_finalise
    , createSqlAggregate (-1) "min_key"
        min_key_step min_key_finalise
    , createSqlWindow 4 "random_sample" random_sample_step
        random_sample_finalise random_sample_value random_sample_inverse
    ]

wrapSqliteExceptions :: (MonadLogger m, MonadCatch m) => m r -> m r
wrapSqliteExceptions = handle logUnwrappedSqliteException
  where
    logUnwrappedSqliteException exc
        | Just (BenchmarkException e) <- fromException exc
        = throwM e

        | Just e@SqliteException{} <- fromException exc
        = logThrowM $ PrettySqliteException e

        | otherwise
        = throwM exc

foreign import ccall "sqlite-functions.h &randomFun"
    randomFun :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &double_vector_step"
    double_vector_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &double_vector_finalise"
    double_vector_finalise :: FunPtr (Ptr () -> IO ())

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

foreign import ccall "sqlite3.h sqlite3_create_function"
    createFun :: Ptr () -> CString -> CInt -> CInt -> Ptr ()
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
              -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
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

foreign import ccall "sqlite3.h &sqlite3_series_init"
    sqlite3_series_init
        :: FunPtr (Ptr () -> Ptr CString -> Ptr () -> IO CInt)

foreign import ccall "sqlite3.h sqlite3_auto_extension"
    registerAutoExtension :: FunPtr (IO ()) -> IO CInt

initialiseSqlite :: (MonadIO m, MonadThrow m) => m ()
initialiseSqlite = do
    result <- liftIO $ registerAutoExtension (castFunPtr sqlite3_series_init)
    when (result /= sqliteOk) $ do
        throwM . SqliteErrorCode result $ ""

createSqliteFun
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> Ptr ()
    -> m ()
createSqliteFun nArgs strName sqlFun aggStep aggFinal sqlPtr = do
    result <- liftIO . withCString strName $ \name -> do
        createFun sqlPtr name nArgs sqliteAny nullPtr sqlFun aggStep aggFinal
    when (result /= sqliteOk) $ do
        bs <- liftIO $ unpackError sqlPtr
        case decodeUtf8' bs of
            Left exc -> logThrowM $ PrettyUnicodeException exc
            Right txt -> logThrowM $ SqliteErrorCode result txt
  where
    unpackError = getExtendedError >=> getErrorString >=> BS.packCString

createSqlFunction
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> Ptr ()
    -> m ()
createSqlFunction nArgs name funPtr =
  createSqliteFun nArgs name funPtr nullFunPtr nullFunPtr

createSqlAggregate
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => CInt
    -> String
    -> FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())
    -> FunPtr (Ptr () -> IO ())
    -> Ptr ()
    -> m ()
createSqlAggregate nArgs name = createSqliteFun nArgs name nullFunPtr

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
