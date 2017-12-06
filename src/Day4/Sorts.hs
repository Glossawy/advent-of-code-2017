module Day4.Sorts (quickSort, mergeSort) where

{-
  This is just for fun, could've used Data.List to get a builtin
  sort.
-}

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = as ++ (p:bs)
  where as = quickSort $ filter (<= p) xs
        bs = quickSort $ filter ( > p) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <=y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs  = merge (mergeSort as) (mergeSort bs)
  where (as, bs) = splitAt n xs
        n = length xs `div` 2
