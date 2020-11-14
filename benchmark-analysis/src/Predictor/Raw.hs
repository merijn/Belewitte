{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Predictor.Raw (RawPredictor(..), predictorToCxx) where

import qualified Data.Conduit.Combinators as C
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum(..))
import Data.String.Interpolate.IsString (i)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Data.Vector.Storable as VS
import Text.Read (readMaybe)

import Core
import Model (Model(..), TreeNode(..))
import Model.Stats (UnknownSet(..))
import Schema
import Sql (Filter, (==.))
import qualified Sql

data RawPredictor = RawPredictor
    { rawPredictorId :: Key PredictionModel
    , rawPredictorName :: Text
    , rawPredictorModel :: Model
    , rawPredictorUnknownSets :: Map Int64 UnknownSet
    , rawPredictorDefaultImpl :: Int
    , rawPredictorMispredictionStrategy :: Int -> Int
    }

predictorToCxx :: RawPredictor -> SqlM LT.Text
predictorToCxx RawPredictor{..} = toLazyText <$> do
    propEntries <- mkPropertyMapping rawPredictorId

    let (decisionTree, newLabels) = relabelDecisionTree rawPredictorModel
                                        rawPredictorMispredictionStrategy

    implEntries <- mkImplMapping newLabels rawPredictorDefaultImpl

    return . mconcat $ intersperse "\n"
        [ preamble rawPredictorName
        , propEntries
        , implEntries
        , decisionTree
        , finale
        ]

preamble :: Text -> Builder
preamble predictorName = [i|#include <functional>
#include <map>
#include <string>
#include <vector>
#include <sys/types.h>

extern "C" const char *predictorName;
extern "C" const char *predictorName = "#{predictorName}";

struct tree_t {
    double threshold;
    int32_t idx, left, right;
};|]

finale :: Builder
finale = [i|
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

mkPropertyMapping :: Key PredictionModel -> SqlM Builder
mkPropertyMapping modelId = do
    (Sum numProps, propEntries) <-
        Sql.selectSource propFilter [] $ C.foldMapM (mkPropIdx . entityVal)

    return $ [i|
static double properties[#{numProps}];

extern "C" const std::map<std::string,std::reference_wrapper<double>> propNames;
extern "C" const std::map<std::string,std::reference_wrapper<double>>
propNames = {
|] <> propEntries <> [i|};|]
  where
    propFilter :: [Filter ModelProperty]
    propFilter = [ModelPropertyModelId ==. modelId]

    mkPropIdx :: ModelProperty -> SqlM (Sum Int, Builder)
    mkPropIdx ModelProperty{..} = do
        name <- propertyNameProperty <$> Sql.getJust modelPropertyPropId
        return (Sum 1, propEntry name)
      where
        propEntry :: Text -> Builder
        propEntry propName = mconcat
            [ "    { \"", fromText propName
            , "\", std::ref(properties[", decimal modelPropertyPropertyIdx
            , "]) },\n"
            ]

mkImplMapping :: Map (Key Implementation) Int -> Int -> SqlM Builder
mkImplMapping implIndices defImpl = do
    implEntries <- fold <$> M.traverseWithKey lookupName implIndices

    let defaultImpl = implIndices M.! toSqlKey (fromIntegral defImpl)

    return $ [i|
extern "C" const int32_t default_impl;
extern "C" const int32_t default_impl = #{defaultImpl};

extern "C" const std::vector<std::tuple<std::string,int64_t,size_t,size_t,size_t>> implNames;
extern "C" const std::vector<std::tuple<std::string,int64_t,size_t,size_t,size_t>>
implNames = {
|] <> implEntries <> [i|};|]
  where
    lookupName :: Key Implementation -> Int -> SqlM Builder
    lookupName implId idx = do
        implName <- implementationName <$> Sql.getJust implId
        let (kernelName, warpConfig) = kernelConfig implName
        return $ mconcat
            [ "    std::make_tuple( \"" , kernelName , "\", " 
            , fromText (showSqlKey implId), ", ", decimal idx
            , ", ", warpConfig, " ),\n" ]

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

relabelDecisionTree
    :: Model -> (Int -> Int) -> (Builder, Map (Key Implementation) Int)
relabelDecisionTree (Model tree) mispredictionStrategy =
    (decisionTree, newLabels)
  where
    (array, newLabels) = VS.foldl' relabel (mempty, M.empty) tree

    decisionTree = "\nstatic tree_t tree[] = {\n" <> array <> "};"

    relabel
        :: (Builder, Map (Key Implementation) Int)
        -> TreeNode
        -> (Builder, Map (Key Implementation) Int)
    relabel (!out, !imap) Node{..}
      | leftNode /= -1 || rightNode == -1 = (treeRow rightNode, imap)
      | actualImpl < 0 = (treeRow (-1), imap)
      | otherwise = (treeRow newVal, newMap)
      where
        treeRow :: Int -> Builder
        treeRow x = out <> mconcat
            [ "    { "
            , realFloat threshold
            , ", "
            , decimal propIdx
            , ", "
            , decimal leftNode
            , ", "
            , decimal x
            , " },\n"
            ]

        actualImpl :: Int
        actualImpl | rightNode < 0 = mispredictionStrategy rightNode
                   | otherwise = rightNode

        implKey :: Key Implementation
        implKey = toSqlKey (fromIntegral actualImpl)

        update :: Maybe Int -> (Int, Maybe Int)
        update val = case val of
            Just v -> (v, val)
            Nothing -> let v = M.size imap in (v, Just v)

        (newVal, newMap) = M.alterF update implKey imap
