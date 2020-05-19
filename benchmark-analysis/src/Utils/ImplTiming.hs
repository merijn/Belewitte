{-# LANGUAGE RecordWildCards #-}
module Utils.ImplTiming where

import Data.Int (Int64)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

data ImplTiming = ImplTiming
    { implTimingImpl :: {-# UNPACK #-} !Int64
    , implTimingTiming :: {-# UNPACK #-} ! Double
    }

liftImplTiming
    :: (Double -> Double -> Double) -> ImplTiming -> ImplTiming -> ImplTiming
liftImplTiming f (ImplTiming i1 v1) (ImplTiming i2 v2)
   | i1 == i2 = ImplTiming i1 (f v1 v2)
   | otherwise = error $ mconcat
        [ "Shouldn't happen! Found: " , show i1, " and ", show i2]

instance Show ImplTiming where
    show ImplTiming{..} = ('(':) . shows implTimingImpl . (',':)
                        . shows implTimingTiming $ ")"

instance Storable ImplTiming where
    sizeOf _ = sizeOf (0 :: Int64) + sizeOf (0.0 :: Double)
    alignment _ = alignment (0.0 :: Double)
    peek ptr = ImplTiming <$> peek (castPtr ptr)
                          <*> peekByteOff ptr (sizeOf (0 :: Int64))

    poke ptr ImplTiming{..} = do
        poke (castPtr ptr) implTimingImpl
        pokeByteOff ptr (sizeOf (0 :: Int64)) implTimingTiming
