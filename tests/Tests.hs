module Main where
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)

import qualified InverseCaptchaTest as ICT
import ChecksumTest (corruptionChecksumTestGroup)
import SpiralMemoryTest (spiralMemoryTestGroup)
import HighEntropyPassphrasesTest (highEntropyPassphrasesTestGroup)
import TestUtils (testTreeWithData)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    " Advent of Code 2017"
    [
      testTreeWithData ["Day1", "inversecaptcha"] invCaptchaTests,
      corruptionChecksumTestGroup,
      spiralMemoryTestGroup,
      highEntropyPassphrasesTestGroup
    ]

invCaptchaTests :: IO String -> TestTree
invCaptchaTests testData =
  testGroup
    "Day 1 - Inverse Captcha"
    [
      testCase "Part One Holds" $ testData >>= ICT.partOneOfPuzzleHolds,
      testCase "Part Two Holds" $ testData >>= ICT.partTwoOfPuzzleHolds
    ]
