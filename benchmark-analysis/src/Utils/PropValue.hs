{-# LANGUAGE RecordWildCards #-}
module Utils.PropValue where

import Data.Int (Int64)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

data PropValue = PropValue
    { propValuePropId :: {-# UNPACK #-} !Int64
    , propValueValue :: {-# UNPACK #-} ! Double
    }

instance Show PropValue where
    show PropValue{..} = show propValueValue

instance Storable PropValue where
    sizeOf _ = sizeOf (0 :: Int64) + sizeOf (0.0 :: Double)
    alignment _ = alignment (0.0 :: Double)
    peek ptr = PropValue <$> peek (castPtr ptr)
                         <*> peekByteOff ptr (sizeOf (0 :: Int64))

    poke ptr PropValue{..} = do
        poke (castPtr ptr) propValuePropId
        pokeByteOff ptr (sizeOf (0 :: Int64)) propValueValue
