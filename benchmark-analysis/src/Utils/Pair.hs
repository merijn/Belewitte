{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Pair where

data Pair a = Pair { regular :: !a, external :: !a }
    deriving (Functor, Show)

instance Applicative Pair where
    pure x = Pair x x
    Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

toPair :: (a -> c) -> (b -> c) -> (a, b) -> Pair c
toPair f g (a, b) = Pair (f a) (g b)

mergePair :: Semigroup m => Pair m -> m
mergePair Pair{..} = regular <> external

mapFirst :: (a -> a) -> Pair a -> Pair a
mapFirst f (Pair a b) = Pair (f a) b

mapSecond :: (a -> a) -> Pair a -> Pair a
mapSecond f (Pair a b) = Pair a (f b)
