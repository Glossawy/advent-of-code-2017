module RecursiveCircusTest (recursiveCircusTestGroup) where

import Day7.RecursiveCircus

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import TestUtils (testTreeWithData)

testFindBaseProgram :: String -> String -> Assertion
testFindBaseProgram expected input = expected @=? findBaseProgram input

testFindBalancingWeight :: Int -> String -> Assertion
testFindBalancingWeight expected input = expected @=? findBalanceResolvingWeight input

recursiveCircusTestGroup :: TestTree
recursiveCircusTestGroup =
  testGroup
    "Day 7 - Recursive Circus"
    [
      testTreeWithData ["Day7", "example-input"] exampleTestGroup
     ,testTreeWithData ["Day7", "puzzle-input"] puzzleTestGroup
    ]

exampleTestGroup :: IO String -> TestTree
exampleTestGroup testInput =
  testGroup
    "Example Cases"
    [
      testCase "Base Program is tknk" $ testInput >>= testFindBaseProgram "tknk"
     ,testCase "Balancing Resolving Weight is 60" $ testInput >>= testFindBalancingWeight 60
    ]

puzzleTestGroup :: IO String -> TestTree
puzzleTestGroup testInput =
  testGroup
    "Puzzle Cases"
    [
      testCase "Base Program is rqwgj" $ testInput >>= testFindBaseProgram "rqwgj"
     ,testCase "Balancing Resolving Weight is 333" $ testInput >>= testFindBalancingWeight 333
    ]
