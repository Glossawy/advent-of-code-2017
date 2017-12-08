module MemoryReallocationTest (memoryReallocationTestGroup) where

import Day6.MemoryReallocation

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

parseRawInputIO :: IO String -> IO MemArray
parseRawInputIO txt = map read . words <$> txt

allocatorTestCase :: (Maybe (Int, Int) -> Maybe Int) -> Int -> MemArray -> Assertion
allocatorTestCase fn expected mem = Just expected @=? fn (findCycleList mem)

addMaybes :: Maybe (Int, Int) -> Maybe Int
addMaybes Nothing = Nothing
addMaybes (Just (a, b)) = Just (a + b)

fstMaybe :: Maybe (Int, Int) -> Maybe Int
fstMaybe = fmap fst

memoryReallocationTestGroup :: IO String -> TestTree
memoryReallocationTestGroup rawData =
  let testData = parseRawInputIO rawData
  in
  testGroup
    "Day 6 - Memory Reallocation"
    [
      seeTwiceTestGroup testData,
      seeAgainTestGroup testData
    ]

seeTwiceTestGroup :: IO MemArray -> TestTree
seeTwiceTestGroup testData =
  testGroup
    "Part 1 - Steps to see same configuration twice"
    [
      testCase "Part 1 Example" $ allocatorTestCase addMaybes 5 [0, 2, 7, 0],
      testCase "Part 1 Solution" $ testData >>= allocatorTestCase addMaybes 7864
    ]

seeAgainTestGroup :: IO MemArray -> TestTree
seeAgainTestGroup testData =
  testGroup
    "Part 2 - Steps to see value again"
    [
      testCase "Part 2 Example" $ allocatorTestCase fstMaybe 4 [0, 2, 7, 0],
      testCase "Part 2 Solution" $ testData >>= allocatorTestCase fstMaybe 1695
    ]
