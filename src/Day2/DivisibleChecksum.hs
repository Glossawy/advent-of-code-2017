module Day2.DivisibleChecksum where

import Day2.Checksum

divisibleChecksum :: (Ord a, Integral a) => Spreadsheet a -> a
divisibleChecksum = sum . map (uncurry div . findDivisiblePair)

findDivisiblePair :: (Ord a, Integral a) => Row a -> (a, a)
findDivisiblePair = head . filter divisibleNotEqual . cartesianPow
  where divisibleNotEqual (x, y) = x /= y && x `mod` y == 0
        cartesianPow l = [(x, y) | x <- l, y <- l]

findDivisiblePartner :: (Ord a, Integral a) => a -> Row a -> a
findDivisiblePartner x = head . filter ((== 0) . (x `mod`)) . filter (/= x)
