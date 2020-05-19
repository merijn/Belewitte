{-# LANGUAGE LambdaCase #-}
module Utils.Conduit (foldGroup) where

import Data.Function (fix)
import Data.Conduit (ConduitT, Void)
import Data.Conduit as C
import qualified Data.Conduit.Combinators as C

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
