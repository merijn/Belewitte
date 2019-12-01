{-# LANGUAGE RecordWildCards #-}
module Utils.ImplTiming where

import Data.Int (Int64)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

data ImplTiming = ImplTiming
    { implTimingImpl :: {-# UNPACK #-} !Int64
    , implTimingTiming :: {-# UNPACK #-} ! Double
    }

instance Show ImplTiming where
    show ImplTiming{..} = ('(':) . shows implTimingImpl . (',':)
                        . shows implTimingTiming $ ")"

instance Storable ImplTiming where
    sizeOf _ = sizeOf (undefined :: Int64) + sizeOf (undefined :: Double)
    alignment _ = alignment (undefined :: Double)
    peek ptr = ImplTiming <$> peek (castPtr ptr)
                          <*> peekByteOff ptr (sizeOf (undefined :: Int64))

    poke ptr ImplTiming{..} = do
        poke (castPtr ptr) implTimingImpl
        pokeByteOff ptr (sizeOf (undefined :: Int64)) implTimingTiming
