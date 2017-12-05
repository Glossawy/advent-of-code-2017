module SpiralMemoryTest where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase)

import Day3.SpiralMemory
import Day3.Math
import Day3.UlamCursor

spiralMemoryTestGroup :: TestTree
spiralMemoryTestGroup =
  testGroup
    "Day 3 - Spiral Memory"
    [
      mathUtilityTestGroup,
      cursorTestGroup,
      puzzleTestGroup
    ]

mathUtilityTestGroup :: TestTree
mathUtilityTestGroup =
  testGroup
    "Math Utilities"
    [
      testCase "ManhattanDist(<  0, 0>, < 15, 10>) =  25" $ (25 :: Int) @=? manhattan zeroVect2D (Vect2D 15 10),
      testCase "ManhattanDist(<-20, 5>, <300, 20>) = 335" $ (335 :: Int) @=? manhattan (Vect2D (-20) 5) (Vect2D 300 20),
      testCase "ManhattanDist(<  0, 0>, <  0,  0>) =   0" $ (0 :: Int) @=? manhattan zeroVect2D zeroVect2D
    ]


firstFifteen :: [Position]
firstFifteen = map vectFromTuple [(0, 0), (1, 0), (1, 1), (0, 1), (-1, 1),
                                  (-1, 0), (-1, -1), (0, -1), (1, -1), (2, -1),
                                  (2, 0), (2, 1), (2, 2), (1, 2), (0, 2)]

cursorTestGroup :: TestTree
cursorTestGroup =
  testGroup
    "Cursor"
    [
      testCase "Generates Advent Ulam Spiral (First 15)" $ firstFifteenEast @=? ulamIterate 15
    ]
  where ulamIterate n = map getPosition . take n $ iterate transition initialUlamCursor
        getPosition (UlamCursor pos _) = pos
        firstFifteenEast = map vectFromTuple [(0, 0), (1, 0), (1, 1), (0, 1), (-1, 1),
                                              (-1, 0), (-1, -1), (0, -1), (1, -1), (2, -1),
                                              (2, 0), (2, 1), (2, 2), (1, 2), (0, 2)]

puzzleTestGroup :: TestTree
puzzleTestGroup =
  testGroup
    "Spiral Distance"
    [
      testCase "12 found at < 2,  1>" $ Just (Vect2D 2 1) @=? spiralFind 12,
      testCase "22 found at <-1, -2>" $ Just (vnegate (Vect2D 1 2)) @=? spiralFind 22,
      testCase "1 found at <0, 0>" $ Just zeroVect2D @=? spiralFind 1,
      testCase "0 not found" $ Nothing @=? spiralFind 0,

      testGroup
        "Cursor-Based Approach"
        [
          testCase "0 fails" $ Nothing @=? spiralDistanceFromOrigin 0,
          testCase "1 is 0 steps away" $ Just 0 @=? spiralDistanceFromOrigin 1,
          testCase "12 is 3 steps away" $ Just 3 @=? spiralDistanceFromOrigin 12,
          testCase "23 is 2 steps away" $ Just 2 @=? spiralDistanceFromOrigin 23,
          testCase "1024 is 31 steps away" $ Just 31 @=? spiralDistanceFromOrigin 1024,
          testCase "368078 is 371 steps away" $ Just 371 @=? spiralDistanceFromOrigin 368078
        ],

      testGroup
        "Explicit Form Approach"
        [
          testCase "0 fails" $ Nothing @=? spiralSteps 0,
          testCase "1 is 0 steps away" $ Just 0 @=? spiralSteps 1,
          testCase "12 is 3 steps away" $ Just 3 @=? spiralSteps 12,
          testCase "23 is 2 steps away" $ Just 2 @=? spiralSteps 23,
          testCase "1024 is 31 steps away" $ Just 31 @=? spiralSteps 1024,
          testCase "368078 is 371 steps away" $ Just 371 @=? spiralSteps 368078
        ]
    ]
