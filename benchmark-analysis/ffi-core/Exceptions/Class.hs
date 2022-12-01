{-# LANGUAGE GADTs #-}
module Exceptions.Class
    ( BenchmarkException(..)
    , toBenchmarkException
    , fromBenchmarkException
    , logThrowM
    ) where

import Control.Monad.Catch (Exception(..), MonadThrow, SomeException, throwM)
import Control.Monad.Logger (MonadLogger, logErrorN)
import Data.Typeable (Typeable, cast)

import Pretty (Pretty(pretty))
import qualified Pretty

data BenchmarkException where
    BenchmarkException
        :: (Exception e, Pretty e) => Bool -> e -> BenchmarkException
    deriving (Typeable)

instance Show BenchmarkException where
    show (BenchmarkException _ e) = show e

instance Exception BenchmarkException where
    displayException (BenchmarkException _ e) = show $ pretty e

toBenchmarkException :: (Exception e, Pretty e) => e -> SomeException
toBenchmarkException = toException . BenchmarkException False

fromBenchmarkException :: (Exception e, Pretty e) => SomeException -> Maybe e
fromBenchmarkException exc = do
    BenchmarkException _ e <- fromException exc
    cast e

logThrowM :: (Exception e, MonadLogger m, MonadThrow m, Pretty e) => e -> m r
logThrowM exc = do
    logErrorN . Pretty.renderStrict . layoutException $ exc
    case fromException (toException exc) of
        Just (BenchmarkException _ e) -> throwM $ BenchmarkException True e
        Nothing -> throwM exc
  where
    layoutException :: Pretty e => e -> Pretty.SimpleDocStream a
    layoutException = Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty
