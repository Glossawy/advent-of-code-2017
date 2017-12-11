module StreamProcessingTest where

import Day9.StreamProcessing

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import TestUtils (testTreeWithData)

testGroupScore :: Int -> String -> Assertion
testGroupScore expected = (expected @=?) . groupScore . getStreamStats

testGarbageScore :: Int -> String -> Assertion
testGarbageScore expected = (expected @=?) . garbageFound . getStreamStats

streamProcessingTestGroup :: TestTree
streamProcessingTestGroup =
  testGroup
    "Day 9 - Stream Processing"
    [
      exampleTestGroup
     ,testTreeWithData ["Day9", "puzzle-input"] puzzleTestGroup
    ]

exampleTestGroup :: TestTree
exampleTestGroup =
  testGroup
  "Example Cases"
  [
    testCase "Example 1" $ let s = getStreamStats "{{{},{},{{}}}}" in (16, 0) @=? (groupScore s, garbageFound s)
   ,testCase "Example 2" $ let s = getStreamStats "<{o\"i!a,<{i<a>" in (0, 10) @=? (groupScore s, garbageFound s)
   ,testCase "Example 3" $ let s = getStreamStats "{{<a!>},{<a!>},{<a!>},{<ab>}}" in (3, 17) @=? (groupScore s, garbageFound s)
  ]

puzzleTestGroup :: IO String -> TestTree
puzzleTestGroup testInput =
  testGroup
  "Puzzle Cases"
  [
    testCase "Group Score is 11089" $ testInput >>= testGroupScore 11089
   ,testCase "Garbage Score is 5288" $ testInput >>= testGarbageScore 5288
  ]
