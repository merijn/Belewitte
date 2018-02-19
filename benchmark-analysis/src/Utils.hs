{-# LANGUAGE MagicHash #-}
module Utils(byteStringToVector) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.Storable (Storable, sizeOf)
import GHC.Exts (Int(I#), plusAddr#)
import GHC.ForeignPtr (ForeignPtr(..))

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

byteStringToVector :: Storable a => ByteString -> Vector a
byteStringToVector bs = v
  where
    v = VS.unsafeFromForeignPtr0 (plusForeignPtr fptr off) (len `div` elemSize)
    (fptr, off, len) = BS.toForeignPtr bs
    elemSize = sizeOf (undefined `asTypeOf` VS.head v)
