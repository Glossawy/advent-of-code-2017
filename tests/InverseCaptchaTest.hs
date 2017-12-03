module InverseCaptchaTest (partOneOfPuzzleHolds, partTwoOfPuzzleHolds) where

import Day1.InverseCaptcha (inverseCaptcha, inverseCaptchaHalfsies)
import TestUtils (strip)

import Data.Monoid
import Test.Tasty.HUnit ((@=?), Assertion)

partOneOfPuzzleHolds :: String -> Assertion
partOneOfPuzzleHolds = (1175 @=?) . actualValueFor inverseCaptcha

partTwoOfPuzzleHolds :: String -> Assertion
partTwoOfPuzzleHolds = (1166 @=?) . actualValueFor inverseCaptchaHalfsies

actualValueFor :: ([Sum Int] -> Sum Int) -> String -> Int
actualValueFor testFn = getSum . testFn . cleanInput

cleanInput :: String -> [Sum Int]
cleanInput = map charToSum . strip
  where charToSum = Sum . read . (:[])
