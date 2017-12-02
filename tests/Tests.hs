module Main where
import Test.Tasty (defaultMain, testGroup, TestTree, withResource)
import Test.Tasty.HUnit (testCase)

import qualified InverseCaptchaTest as ICT (partOneOfPuzzleHolds, partTwoOfPuzzleHolds)
import TestUtils (readDataFile)

main :: IO ()
main = do
  let release = const (return ())
  defaultMain $ withResource (readDataFile ["Day1", "inversecaptcha"]) release unitTests

unitTests :: IO String -> TestTree
unitTests invCaptchaAction =
  testGroup
    " Advent of Code 2017 - Day 1 - Inverse Captcha"
    [
      testCase "Part One Holds" $ invCaptchaAction >>= ICT.partOneOfPuzzleHolds,
      testCase "Part Two Holds" $ invCaptchaAction >>= ICT.partTwoOfPuzzleHolds
    ]
