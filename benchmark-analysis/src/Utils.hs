{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonadFailDesugaring #-}
module Utils(byteStringToVector, vectorToByteString) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import Foreign.Storable (Storable, sizeOf)
import GHC.Exts (Int(I#), plusAddr#)
import GHC.ForeignPtr (ForeignPtr(..), castForeignPtr)

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

byteStringToVector :: (Storable a, Vector v a) => ByteString -> v a
byteStringToVector bs = VS.convert v
  where
    v = VS.unsafeFromForeignPtr0 (plusForeignPtr fptr off) (len `div` elemSize)
    (fptr, off, len) = BS.toForeignPtr bs
    elemSize = sizeOf (undefined `asTypeOf` VS.head v)

vectorToByteString :: (Storable a, Vector v a) => v a -> ByteString
vectorToByteString vec = BS.fromForeignPtr (castForeignPtr fptr) 0 byteLen
  where
    (fptr, len) = VS.unsafeToForeignPtr0 . VS.convert $ vec
    byteLen = len * sizeOf (undefined `asTypeOf` V.head vec)
