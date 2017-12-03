module Day2.CorruptionChecksum where

import Day2.Checksum
import Data.Tuple (uncurry)

minMaxChecksum :: (Ord a, Num a) => Spreadsheet a -> a
minMaxChecksum = sum . map (abs . uncurry (-) . findMinMaxPair)

findMinMaxPair :: (Ord a) => [a] -> (a, a)
findMinMaxPair [x] = (x, x)
findMinMaxPair xs = (minimum xs, maximum xs)
