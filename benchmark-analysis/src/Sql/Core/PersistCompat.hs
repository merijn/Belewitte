{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sql.Core.PersistCompat
    (unsafeAcquireSqlConnFromPool, acquireSqlConnFromPool) where

#if MIN_VERSION_persistent(2,12,0)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MonadReader
import Data.Acquire (Acquire, ReleaseType(..), mkAcquireType)
import Data.Pool as Pool
import Database.Persist.Sql (BackendCompatible, SqlBackend, acquireSqlConn)

unsafeAcquireSqlConnFromPool
    :: forall backend m
     . (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
unsafeAcquireSqlConnFromPool = do
    pool <- MonadReader.ask

    let freeConn :: (backend, LocalPool backend) -> ReleaseType -> IO ()
        freeConn (res, localPool) relType = case relType of
            ReleaseException -> Pool.destroyResource pool localPool res
            _ -> Pool.putResource localPool res

    return $ fst <$> mkAcquireType (Pool.takeResource pool) freeConn

acquireSqlConnFromPool
    :: (MonadReader (Pool backend) m, BackendCompatible SqlBackend backend)
    => m (Acquire backend)
acquireSqlConnFromPool = do
    connFromPool <- unsafeAcquireSqlConnFromPool
    return $ connFromPool >>= acquireSqlConn
#else
import Database.Persist.Sqlite
    (unsafeAcquireSqlConnFromPool, acquireSqlConnFromPool)
#endif
