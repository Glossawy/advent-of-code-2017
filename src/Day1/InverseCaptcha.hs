module Day1.InverseCaptcha (inverseCaptcha, inverseCaptchaHalfsies) where

import Data.Tuple (uncurry)
import Data.Monoid
import Util.SplitList (splitListInTwo)

-- Using monoids to support arbitrary types with an associative binary operator
-- See: String, Int, etc.

-- Part 1
inverseCaptcha :: (Ord a, Eq a, Monoid a) => [a] -> a
inverseCaptcha = mconcat . matchesNeighbor

matchesNeighbor :: (Ord a, Eq a) => [a] -> [a]
matchesNeighbor [] = []
matchesNeighbor l@(x:_) = findNeighborMatchers [] (l ++ [x])
  where findNeighborMatchers s (a:b:vs)
          | a == b = findNeighborMatchers (a:s) (b:vs)
          | otherwise = findNeighborMatchers s (b:vs)
        findNeighborMatchers s _ = s


-- Part 2
inverseCaptchaHalfsies :: (Ord a, Eq a, Monoid a) => [a] -> a
inverseCaptchaHalfsies = mconcat . uncurry matchesOtherList . splitListInTwo

matchesOtherList :: (Ord a, Eq a, Monoid a) => [a] -> [a] -> [a]
matchesOtherList = findMatching []
  where findMatching s (x:xs) (y:ys)
          | x == y    = findMatching ( x <> y : s) xs ys
          | otherwise = findMatching s xs ys
        findMatching s _ _ = s
