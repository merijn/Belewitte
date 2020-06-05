{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Model
    ( Model
    , mapPropIndices
    , mapImplementations
    , predict
    , predictPropVector
    , dumpCppModel
    , byteStringToModel
    ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Data.Text.Lazy.IO as LT
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Text.Read (readMaybe)

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

mapPropIndices :: (Int -> Maybe Int) -> Model -> Maybe Model
mapPropIndices f (Model vec) = Model <$> VS.mapM changeNode vec
  where
    changeNode :: TreeNode -> Maybe TreeNode
    changeNode node = case f (propIdx node) of
        Just v -> Just node{ propIdx = v }
        Nothing -> Nothing

mapImplementations :: (Int -> Maybe Int) -> Model -> Maybe Model
mapImplementations f (Model vec) = Model <$> VS.mapM changeNode vec
  where
    changeNode :: TreeNode -> Maybe TreeNode
    changeNode node
        | leftNode node /= -1 = Just node
        | otherwise = case f (rightNode node) of
            Just v -> Just node{ rightNode = v }
            Nothing -> Nothing

dumpCppModel
    :: MonadIO m
    => FilePath
    -> Model
    -> Set (Int, Text)
    -> IntMap Text
    -> m ()
dumpCppModel name (Model tree) modelProps implNames =
    liftIO . LT.writeFile name . toLazyText $ [i|#include <functional>
#include <map>
#include <string>
#include <vector>
#include <sys/types.h>

using namespace std;

struct tree_t {
    double threshold;
    int32_t idx, left, right;
};

static tree_t tree[] = {
|] <> decisionTree <> [i|};

static double properties[#{numProps}];

extern "C" const vector<tuple<string,size_t,size_t,size_t>>
implNames = {
|] <> nameTable newLabels <> [i|};

extern "C" const std::map<std::string,std::reference_wrapper<double>>
propNames = {
|] <> propEntries <> [i|};

extern "C" int32_t lookup();
extern "C" int32_t lookup()
{
    int node = 0, left;

    while ((left = tree[node].left) != -1) {
        if (properties[tree[node].idx] <= tree[node].threshold) {
            node = left;
        } else {
            node = tree[node].right;
        }
    }
    return tree[node].right;
}
|]
  where
    numProps = S.size modelProps

    (decisionTree, newLabels) = VS.foldl' relabel (mempty, IM.empty) tree

    nameTable :: IntMap Int -> Builder
    nameTable = foldMap lookupName . sortedTable . IM.toList
      where
        sortedTable :: [(Int, Int)] -> [(Int, Int)]
        sortedTable = sortBy (comparing fst) . map swap
          where
            swap :: (a, b) -> (b, a)
            swap (x, y) = (y, x)

        kernelConfig :: Text -> (Builder, Builder)
        kernelConfig implName
            | "-warp-" `T.isInfixOf` implName
            , Just warp <- readMaybe (T.unpack warpTxt) :: Maybe Int
            , Just chunk <- readMaybe (T.unpack chunkTxt) :: Maybe Int
            = (fromText newName, decimal warp <> ", " <> decimal chunk)
            | "-warp-" `T.isInfixOf` implName || "-warp" `T.isSuffixOf` implName
            = (fromText implName, "32, 32")
            | otherwise = (fromText implName, "0, 0")
          where
            chunks = T.split (=='-') implName

            (revWarpCfg, revRemainder) = splitAt 2 . reverse $ chunks

            [warpTxt, chunkTxt] = reverse revWarpCfg

            newName = T.intercalate "-" . reverse $ revRemainder

        lookupName :: (Int, Int) -> Builder
        lookupName (new, original) = mconcat
                [ "    { \"" , kernelName , "\", " , decimal new
                , ", ", warpConfig, " },\n" ]
          where
            implName = implNames IM.! original
            (kernelName, warpConfig) = kernelConfig implName

    propEntries :: Builder
    propEntries = foldMap propEntry modelProps
      where
        propEntry :: (Int, Text) -> Builder
        propEntry (idx, propName) = mconcat
            [ "    { \"", fromText propName
            , "\", std::ref(properties[", decimal idx
            , "]) },\n"
            ]

    relabel :: (Builder, IntMap Int) -> TreeNode -> (Builder, IntMap Int)
    relabel (!out, !imap) Node{..}
      | leftNode /= -1 = (treeRow rightNode, imap)
      | rightNode < 0 = (treeRow (-1), imap)
      | otherwise = (treeRow newVal, newMap)
      where
        treeRow :: Int -> Builder
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
