module Day6.MemoryReallocation (findCycle,findCycleList, MemBank, MemArray, MemVector) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.List

type MemBank = Int
type MemArray = [MemBank]
type MemVector = Vector MemBank

{- Accumulate increments, apply to Vector where v[maxIdx] = 0 -}
realloc :: MemVector -> MemVector
realloc xs = V.accum (+) v' increments
  where increments = [((maxIx + ix) `mod` n, 1) | ix <- [1..resources] ]
        maxIx = V.maxIndex xs       -- Get index of maximum
        resources = xs ! maxIx      -- Get maximum value
        v' = xs V.// [(maxIx, 0)]   -- Set max index to 0
        n = V.length xs

{- Separate Calculation of lambda and mu from Brent's Cycle Detection Algorithm -}
findCycle :: MemVector -> Maybe (Int, Int)
findCycle mem = do
  lam <- findCycleLength states
  mu <- findIndex (uncurry (==)) $ zip states (drop lam states)
  return (lam, mu)
    where states = iterate realloc mem

{- Calculate length of loop (lambda in Brent's/Floyd's Cycle Detect Algorithm(s)) -}
findCycleLength :: [MemVector] -> Maybe Int
findCycleLength (x:xs) = brentLoop 1 1 x xs
  where brentLoop _ _ _ [] = Nothing
        brentLoop pow lam t (h:hs)
          | t == h      = Just lam
          | pow == lam  = brentLoop (2*pow) 1 h hs
          | otherwise   = brentLoop pow (lam+1) t hs

{- Variant for Lists -}
findCycleList :: MemArray -> Maybe (Int, Int)
findCycleList = findCycle . V.fromList
