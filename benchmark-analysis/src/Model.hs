{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model
    ( Model(..)
    , TreeNode(..)
    , mapPropIndices
    , mapImplementations
    , predict
    , predictPropVector
    , byteStringToModel
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

import Utils.PropValue (PropValue(propValueValue))
import Utils.Vector (byteStringToVector, vectorToByteString)

newtype Model = Model { getModelVector :: VS.Vector TreeNode }

instance PersistField Model where
    toPersistValue (Model vec) = toPersistValue $ vectorToByteString vec
    fromPersistValue val = Model . byteStringToVector <$> fromPersistValue val

instance PersistFieldSql Model where
    sqlType proxy = sqlType $ vectorToByteString . getModelVector <$> proxy

predict :: Model -> Vector Double -> Int
predict (Model tree) props = go (tree `VS.unsafeIndex` 0)
  where
    go :: TreeNode -> Int
    go !Node{..}
        | leftNode == -1 = rightNode
        | belowThreshold = go (tree `VS.unsafeIndex` leftNode)
        | otherwise = go (tree `VS.unsafeIndex` rightNode)
        where
          belowThreshold = props `VS.unsafeIndex` propIdx <= threshold
{-# INLINE predict #-}

predictPropVector :: Model -> Vector PropValue -> Int
predictPropVector (Model tree) props = go (tree `VS.unsafeIndex` 0)
  where
    go :: TreeNode -> Int
    go !Node{..}
        | leftNode == -1 = rightNode
        | belowThreshold = go (tree `VS.unsafeIndex` leftNode)
        | otherwise = go (tree `VS.unsafeIndex` rightNode)
        where
          belowThreshold =
            propValueValue (props `VS.unsafeIndex` propIdx) <= threshold
{-# INLINE predictPropVector #-}

mapModel :: Monad m => (TreeNode -> m TreeNode) -> Model -> m Model
mapModel f (Model vec) = Model <$> VS.mapM f vec

mapPropIndices :: forall m . Monad m => (Int -> m Int) -> Model -> m Model
mapPropIndices f = mapModel changeNode
  where
    changeNode :: TreeNode -> m TreeNode
    changeNode node
        | leftNode node == -1 = pure node
        | otherwise = (\v -> node{ propIdx = v }) <$> f (propIdx node)

mapImplementations
    :: forall m . Monad m => (Int -> m Int) -> Model -> m Model
mapImplementations f = mapModel changeNode
  where
    changeNode :: TreeNode -> m TreeNode
    changeNode node
        | leftNode node /= -1 = pure node
        | otherwise = (\v -> node{ rightNode = v }) <$> f (rightNode node)

data TreeNode
    = Node
      { threshold :: {-# UNPACK #-} !Double
      , propIdx :: {-# UNPACK #-} !Int
      , leftNode :: {-# UNPACK #-} !Int
      , rightNode :: {-# UNPACK #-} !Int
      } deriving (Show)

instance Storable TreeNode where
    sizeOf _ = sizeOf (0.0 :: Double) + 4 * sizeOf (0 :: Int32)
    alignment _ = alignment (0.0 :: Double)
    peek ptr = Node <$> peek (castPtr ptr)
                    <*> (fromIntegral <$> peekInt32 0)
                    <*> (fromIntegral <$> peekInt32 1)
                    <*> (fromIntegral <$> peekInt32 2)
      where
        peekInt32 :: Int -> IO Int32
        peekInt32 n = peekByteOff ptr (doubleSize + n * int32Size)
          where
            doubleSize = sizeOf (0.0 :: Double)
            int32Size = sizeOf (0 :: Int32)
        {-# INLINE peekInt32 #-}

    poke ptr Node{..} = do
        poke (castPtr ptr) threshold
        pokeInt32 0 (fromIntegral propIdx)
        pokeInt32 1 (fromIntegral leftNode)
        pokeInt32 2 (fromIntegral rightNode)
      where
        pokeInt32 :: Int -> Int32 -> IO ()
        pokeInt32 n val = pokeByteOff ptr (doubleSize + n * int32Size) val
          where
            doubleSize = sizeOf (0.0 :: Double)
            int32Size = sizeOf (0 :: Int32)

byteStringToModel :: ByteString -> Model
byteStringToModel = Model . byteStringToVector
