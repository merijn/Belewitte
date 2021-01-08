module Utils.Vector(byteStringToVector, vectorToByteString) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr (castForeignPtr, plusForeignPtr)
import Foreign.Storable (Storable, sizeOf)

byteStringToVector :: Storable a => ByteString -> Vector a
byteStringToVector bs = v
  where
    v = VS.unsafeFromForeignPtr0 (plusForeignPtr fptr off) (len `div` elemSize)
    (fptr, off, len) = BS.toForeignPtr bs
    elemSize = sizeOf $ VS.head v

vectorToByteString :: Storable a => Vector a -> ByteString
vectorToByteString vec = BS.fromForeignPtr (castForeignPtr fptr) 0 byteLen
  where
    (fptr, len) = VS.unsafeToForeignPtr0 vec
    byteLen = len * sizeOf (VS.head vec)
