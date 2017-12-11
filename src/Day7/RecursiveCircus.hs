{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module Day7.RecursiveCircus (findBaseProgram, findBalanceResolvingWeight) where

import           Control.Applicative ((<|>))
import           Data.Char           (isAlpha)
import           Data.Maybe          (mapMaybe, listToMaybe, catMaybes, fromJust)
import           Data.List           (sortOn)
import           Data.Map            ((!))

import qualified Data.Map as Map
import qualified Data.Set as Set

type ProgramName      = String
data ProgTree a       = NilProgram | Program ProgramName a (ProgForest a)
type ProgForest a     = [ProgTree a]
type ProgInfo a       = (a, Set.Set ProgramName)
type ProgDescriptor a = (ProgramName, ProgInfo a)

{- Tree Definitions -}

instance Foldable ProgTree where
  foldMap _ NilProgram = mempty
  foldMap f (Program _ w children) = mappend (f w) $ mconcatWith f children
    where mconcatWith g = mconcat . map (foldMap g)

subProgForest :: ProgTree a -> ProgForest a
subProgForest NilProgram = []
subProgForest (Program _ _ children) = children

weight :: ProgTree a -> Maybe a
weight NilProgram = Nothing
weight (Program _ w _) = Just w

unfoldProgTree :: (ProgramName -> (a, [ProgramName])) -> ProgramName -> ProgTree a
unfoldProgTree f x = let (w, bs) = f x in Program x w (unfoldProgForest f bs)

unfoldProgForest :: (ProgramName -> (a, [ProgramName])) -> [ProgramName] -> ProgForest a
unfoldProgForest f = map (unfoldProgTree f)

{- Solution -}

findRebalancingWeight :: (Num a, Ord a) => ProgTree a -> Maybe a                  -- Part 2
findRebalancingWeight t = listToMaybe unbalancedChildren <|> anomalous
  where unbalancedChildren = mapMaybe findRebalancingWeight $ subProgForest t
        treeToWeightEntry subtree = (sum subtree,) $ catMaybes [weight subtree]
        weightMap = Map.fromListWith (++) . map treeToWeightEntry $ subProgForest t
        anomalous = case sortOn (length . snd) (Map.toList weightMap) of
          [ ]                         -> Nothing                        -- Leaf, No Child Weights
          [_]                         -> Nothing                        -- No Issues (1 weight)
          [(tsum1, [w1]), (tsum2, _)] -> Just (w1 + (tsum2 - tsum1))    -- One Unbalanced Node (2 weights)
          _                           -> error "More than one anomaly"  -- > 2 weights, shouldn't happen

buildProgTree :: Map.Map ProgramName (ProgInfo a) -> (ProgramName, ProgTree a)    -- Part 1
buildProgTree m = (rootProgram, resultTree)
  where allChildren = Set.unions $ getChildList <$> Map.toList m
        getChildList = snd . snd
        rootProgram = Set.findMax $ Map.keysSet m `Set.difference` allChildren
        resultTree = flip unfoldProgTree rootProgram $ \name ->
          let (w, children) = m ! name in (w, Set.toList children)

parseProgram :: String -> ProgDescriptor Int
parseProgram (words -> p:w:ws) = (p, (read w, Set.fromList (filter isAlpha <$> drop 1 ws)))
parseProgram _ = error "parse: Invalid Program String"

parseProgTree :: String -> (ProgramName, ProgTree Int)
parseProgTree = buildProgTree . Map.fromList . map parseProgram . filter (not . null) . lines

findBaseProgram :: String -> String
findBaseProgram = fst . parseProgTree

findBalanceResolvingWeight :: String -> Int
findBalanceResolvingWeight = fromJust . findRebalancingWeight . snd . parseProgTree
