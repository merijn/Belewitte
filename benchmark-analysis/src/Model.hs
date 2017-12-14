{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Model (Model, predict, dumpCppModel, byteStringToModel) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.String.Interpolate.IsString
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Schema (Implementation(..), Text)

newtype Model = Model (VS.Vector TreeNode)

predict :: Model -> VU.Vector Double -> Int
predict (Model tree) props = go (tree `VS.unsafeIndex` 0)
  where
    go :: TreeNode -> Int
    go !Node{..}
        | leftNode == -1 = rightNode
        | belowThreshold = go (tree `VS.unsafeIndex` leftNode)
        | otherwise = go (tree `VS.unsafeIndex` rightNode)
        where
          belowThreshold = props `VU.unsafeIndex` propIdx <= threshold

dumpCppModel
    :: MonadIO m
    => FilePath
    -> Model
    -> Set Text
    -> Set Text
    -> IntMap Implementation
    -> m ()
dumpCppModel name (Model tree) graphProps stepProps implNames =
  liftIO . T.writeFile name . toLazyText $ [i|#include <functional>
#include <map>
#include <string>
#include <sys/types.h>

struct tree_t {
    double threshold;
    int32_t idx, left, right;
};

static tree_t tree[] = {
|] <> decisionTree <> [i|};

static double properties[#{numProps}];

const std::map<std::string,size_t> implNames = {
|] <> nameTable newLabels <> [i|};

const std::map<std::string,std::reference_wrapper<double>> propNames = {
|] <> propEntries <> [i|};

int32_t lookup();
int32_t lookup()
{
    int node = 0, left;

    while ((left = tree[node].left) != -1) {
        if (properties[tree[node].idx] <= tree[node].threshold) {
            node = left;
        } else {
            node = tree[node].right;
        }
    }
    return static_cast<size_t>(tree[node].right);
}
|]
  where
    numProps = S.size graphProps + S.size stepProps

    (decisionTree, newLabels) = VS.foldl' relabel (mempty, IM.empty) tree

    nameTable :: IntMap Int -> Builder
    nameTable = foldMap lookupName . sortedTable . IM.toList
      where
        sortedTable :: [(Int, Int)] -> [(Int, Int)]
        sortedTable = sortBy (comparing fst) . map swap
          where
            swap :: (a, b) -> (b, a)
            swap (x, y) = (y, x)

        lookupName :: (Int, Int) -> Builder
        lookupName (new, original) = case implNames IM.! original of
            Implementation _ implName _ -> mconcat
                [ "    { \"", fromText implName, "\", ", decimal new, " },\n" ]

    propEntries :: Builder
    propEntries = foldMap propEntry . zip [0..] $ props
      where
        props = S.toList graphProps ++ S.toList stepProps

        propEntry :: (Int, Text) -> Builder
        propEntry (idx, propName) = mconcat
            [ "    { \"", fromText propName
            , "\", std::ref(properties[", decimal idx
            , "]) },\n"
            ]

    relabel :: (Builder, IntMap Int) -> TreeNode -> (Builder, IntMap Int)
    relabel (!out, !imap) Node{..}
      | leftNode /= -1 || rightNode == -1 = (treeRow rightNode, imap)
      | otherwise = (treeRow newVal, newMap)
      where
        treeRow x = mconcat
            [ out
            , "    { "
            , realFloat threshold
            , ", "
            , decimal propIdx
            , ", "
            , decimal leftNode
            , ", "
            , decimal x
            , " },\n"
            ]

        update val = case val of
            Just v -> (v, val)
            Nothing -> let v = IM.size imap in (v, Just v)

        (newVal, newMap) = IM.alterF update rightNode imap

data TreeNode
    = Node
      { threshold :: {-# UNPACK #-} !Double
      , propIdx :: {-# UNPACK #-} !Int
      , leftNode :: {-# UNPACK #-} !Int
      , rightNode :: {-# UNPACK #-} !Int
      } deriving (Show)

instance Storable TreeNode where
    sizeOf _ = sizeOf (undefined :: Double) + 4 * sizeOf (undefined :: Int32)
    alignment _ = alignment (undefined :: Double)
    peek ptr = Node <$> peek (castPtr ptr)
                    <*> (fromIntegral <$> peekInt32 0)
                    <*> (fromIntegral <$> peekInt32 1)
                    <*> (fromIntegral <$> peekInt32 2)

      where
        peekInt32 :: Int -> IO Int32
        peekInt32 n = peekByteOff ptr (doubleSize + n * int32Size)
          where
            doubleSize = sizeOf (undefined :: Double)
            int32Size = sizeOf (undefined :: Int32)

    poke ptr Node{..} = do
        poke (castPtr ptr) threshold
        pokeInt32 0 (fromIntegral propIdx)
        pokeInt32 1 (fromIntegral leftNode)
        pokeInt32 2 (fromIntegral rightNode)
      where
        pokeInt32 :: Int -> Int32 -> IO ()
        pokeInt32 n val = pokeByteOff ptr (doubleSize + n * int32Size) val
          where
            doubleSize = sizeOf (undefined :: Double)
            int32Size = sizeOf (undefined :: Int32)

byteStringToModel :: ByteString -> Model
byteStringToModel bs = Model vec
  where
    vec = VS.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = BS.toForeignPtr bs
    scale = (`div` sizeOf (undefined `asTypeOf` VS.head vec))
