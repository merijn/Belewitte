{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MonadFailDesugaring #-}
module SQLiteExts (registerSqlFunctions) where

import Control.Monad ((>=>), when)
import Control.Monad.Catch (MonadThrow, displayException, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger)
import qualified Control.Monad.Logger as Log
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Foreign (Ptr, FunPtr, nullPtr, nullFunPtr)
import Foreign.C (CInt(..), CString, withCString)

registerSqlFunctions
    :: (MonadIO m, MonadLogger m, MonadThrow m) => Ptr () -> m ()
registerSqlFunctions sqlitePtr = mapM_ ($sqlitePtr)
    [ createSqlFunction 1 "random" randomFun
    , createSqlAggregate 3 "double_vector"
        double_vector_step double_vector_finalise
    , createSqlAggregate 3 "int64_vector"
        int64_vector_step int64_vector_finalise
    ]

foreign import ccall "sqlite-functions.h &randomFun"
    randomFun :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &int64_vector_step"
    int64_vector_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &int64_vector_finalise"
    int64_vector_finalise :: FunPtr (Ptr () -> IO ())

foreign import ccall "sqlite-functions.h &double_vector_step"
    double_vector_step :: FunPtr (Ptr () -> CInt -> Ptr (Ptr ()) -> IO ())

foreign import ccall "sqlite-functions.h &double_vector_finalise"
    double_vector_finalise :: FunPtr (Ptr () -> IO ())

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
            Left exc -> do
                Log.logErrorN . T.pack . displayException $ exc
                throwM exc
            Right txt -> Log.logErrorN txt
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
