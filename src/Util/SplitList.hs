module Util.SplitList (splitListInTwo) where

splitListInTwo :: [a] -> ([a], [a])
splitListInTwo xys = (xs, reverse ys)
  where (xs, ys) = splitListInTwo' xys

splitListInTwo' :: [a] -> ([a], [a])
splitListInTwo' [] = ([], [])
splitListInTwo' [x] = ([x], [])
splitListInTwo' (x:xys) = (x:xs, last xys:ys)
  where (xs, ys) = splitListInTwo' $ init xys
