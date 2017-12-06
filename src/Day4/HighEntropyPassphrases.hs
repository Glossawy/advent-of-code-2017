module Day4.HighEntropyPassphrases where

import Day4.Sorts

type Passphrase = String
type Validity = Bool

isValidPassphrase :: (String -> String) -> Passphrase -> Validity
isValidPassphrase f = isValid [] . words
  where isValid _ [] = True
        isValid seen (w:ws)
          | w' `elem` seen = False
          | otherwise      = isValid (w':seen) ws
            where w' = f w

isNonRepetitivePassphrase :: Passphrase -> Validity
isNonRepetitivePassphrase = isValidPassphrase id

isNonAnagramPassphrase :: Passphrase -> Validity
isNonAnagramPassphrase = isValidPassphrase mergeSort

countValidPassphrases :: (Passphrase -> Validity) -> [Passphrase] -> Int
countValidPassphrases vf = length . filter vf
