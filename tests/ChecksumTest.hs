module ChecksumTest where

import TestUtils (strip, readDataFile, testTreeWithData)
import Day2.CorruptionChecksum (minMaxChecksum, findMinMaxPair)
import Day2.DivisibleChecksum (divisibleChecksum)

import Data.Monoid

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), Assertion, testCase)

testFindMinMaxPair :: Assertion
testFindMinMaxPair = (1, 100) @=? findMinMaxPair xs
  where xs = [5, 23, 12, 1, 20, 10, 30, 100]

testMinMaxChecksum :: Int -> String -> Assertion
testMinMaxChecksum expected = (expected @=?) . actualValueFor minMaxChecksum

testDivisibleChecksum :: Int -> String -> Assertion
testDivisibleChecksum expected = (expected @=?) . actualValueFor' (divisibleChecksum . map (map getSum))

actualValueFor' :: ([[Sum Int]] -> Int) -> String -> Int
actualValueFor' testFn = testFn . cleanInput

actualValueFor :: ([[Sum Int]] -> Sum Int) -> String -> Int
actualValueFor testFn = getSum . testFn . cleanInput

cleanInput :: String -> [[Sum Int]]
cleanInput = map (map wordToSum) . cleanRaw
  where wordToSum = Sum . read
        cleanRaw = map words . lines . strip


-- Test Cases

corruptionChecksumTestGroup :: TestTree
corruptionChecksumTestGroup =
  testGroup
    "Day 2 - Corruption Checksum"
    [
      utilTestGroup,
      exampleTestGroup,
      testTreeWithData ["Day2", "corruption-checksum"] puzzleTestGroup
    ]

utilTestGroup :: TestTree
utilTestGroup =
  testGroup
    "Utilities"
    [
      testCase "Find (Min, Max) Pair of List" testFindMinMaxPair
    ]

exampleTestGroup :: TestTree
exampleTestGroup =
  testGroup
    "Example Cases"
    [
      testCase "Part One Example Holds" $ readDataFile ["Day2", "corruption-example"] >>= testMinMaxChecksum 18,
      testCase "Part Two Example Holds" $ readDataFile ["Day2", "divisible-example"] >>= testDivisibleChecksum 9
    ]

puzzleTestGroup :: IO String -> TestTree
puzzleTestGroup testData =
  testGroup
    "Puzzle Cases"
    [
      testCase "Part One Puzzle Solution Holds" $ testData >>= testMinMaxChecksum 48357,
      testCase "Part Two Puzzle Solution Holds" $ testData >>= testDivisibleChecksum 351
    ]
