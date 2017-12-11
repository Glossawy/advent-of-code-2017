{-# LANGUAGE CPP #-}
module JumpOnlyCpuTest where

import Day5.JumpOnlyCpu

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, Assertion)

import TestUtils (testTreeWithData)

testInterpreter :: JmpInterpreter -> Int -> [Instruction] -> Assertion
testInterpreter interp expected = (expected @=?) . interp


jumpOnlyCpuTestGroup :: TestTree
jumpOnlyCpuTestGroup = testTreeWithData ["Day5", "puzzle-instruction-set"] tests
  where tests :: IO String -> TestTree
        tests rawTestData =
          let testData = do d <- rawTestData; return $ map read . filter (/= "") $ lines d :: IO [Int]
          in testGroup
            "Day 5 - A Maze of Twisty Trampolines, All Alike"
            [
              partOneTestGroup testData
             ,partTwoTestGroup testData
            ]

partOneTestGroup :: IO [Int] -> TestTree
partOneTestGroup inputData =
  testGroup
    "Part 1 - Increment Instruction Interpret"
    [
      testCase "Part 1 Example" $ 5 @=? incrementOnlyInterpreter [0, 3, 0, 1, -3]
     ,testCase "Part 1 Solution" $ inputData >>= testInterpreter incrementOnlyInterpreter 372139
    ]

partTwoTestGroup :: IO [Int] -> TestTree
partTwoTestGroup inputData =
  testGroup
    "Part 2 - Increment < 3, Decement >= 3 Interpreter"
    [
      testCase "Part 2 Example" $ 10 @=? threshold3Interpreter [0, 3, 0, 1, -3]
#ifndef CIBUILD
     ,testCase "Part 2 Solution" $ inputData >>= testInterpreter threshold3Interpreter 29629538
#else
     ,testCase "Part 2 Solution (Skipped for CI)" $ 1 @=? 1
#endif
    ]
