module UnusualRegistersTest (unusualRegistersTestGroup) where

import Day8.UnusualRegisters

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import TestUtils (testTreeWithData)

testGreatestValuedRegister :: Int -> String -> Assertion
testGreatestValuedRegister expected = (expected @=?) . findGreatestValuedRegister

testGreatestAlltimeRegister :: Int -> String -> Assertion
testGreatestAlltimeRegister expected = (expected @=?) . findGreatestAlltimeRegister

unusualRegistersTestGroup :: TestTree
unusualRegistersTestGroup =
  testGroup
    "Day 8 - Unusual Registers"
    [
      testTreeWithData ["Day8", "example-input"] exampleTestGroup
     ,testTreeWithData ["Day8", "puzzle-input"] puzzleTestGroup
    ]

exampleTestGroup :: IO String -> TestTree
exampleTestGroup testInput =
  testGroup
    "Example Cases"
    [
      testCase "Greatest Valued Register is 1" $ testInput >>= testGreatestValuedRegister 1
     ,testCase "Greatest Alltime Register is 10" $ testInput >>= testGreatestAlltimeRegister 10
    ]

puzzleTestGroup :: IO String -> TestTree
puzzleTestGroup testInput =
  testGroup
    "Puzzle Cases"
    [
      testCase "Greatest Valued Register is 4877" $ testInput >>= testGreatestValuedRegister 4877
     ,testCase "Greatest Alltime Register is 5471" $ testInput >>= testGreatestAlltimeRegister 5471
    ]
