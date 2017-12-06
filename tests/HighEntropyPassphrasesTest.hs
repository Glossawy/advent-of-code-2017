module HighEntropyPassphrasesTest where

import Day4.Sorts
import Day4.HighEntropyPassphrases

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

import TestUtils (testTreeWithData)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), testCase, (@?), Assertion)

shuffleList :: [a] -> Int -> [a]
shuffleList xs n = shuffle' xs (length xs) (mkStdGen n)

testCountValidPassphrases :: Int -> (Passphrase -> Validity) -> [Passphrase] -> Assertion
testCountValidPassphrases expected f = (expected @=?) . countValidPassphrases f

highEntropyPassphrasesTestGroup :: TestTree
highEntropyPassphrasesTestGroup =
  testGroup
    "Day 4 - High Entropy Passphrases"
    [
      sortsTestGroup,
      testTreeWithData ["Day4", "p1-passphrases"] puzzleTestGroup
    ]

sortsTestGroup :: TestTree
sortsTestGroup =
  testGroup
    "Sort Utilites"
    [
      testGroup "QuickSort"
        [
          testCase "Contrived Case 1" $ let xs = [1..6] :: [Int] in (xs @=?) $ quickSort $ shuffleList xs 1,
          testCase "Contrived Case 2" $ let xs = [30,60..3000] :: [Int] in (xs @=?) $ quickSort $ shuffleList xs 2,
          testCase "Empty List" $ let xs = [] :: [Int] in xs @=? quickSort xs,
          testCase "Singleton List" $ let xs = ['b'] in xs @=? quickSort xs
        ],
      testGroup "MergeSort"
        [
          testCase "Contrived Case 1" $ let xs = [1..6] :: [Int] in (xs @=?) $ mergeSort $ shuffleList xs 1,
          testCase "Contrived Case 2" $ let xs = [30,60..3000] :: [Int] in (xs @=?) $ mergeSort $ shuffleList xs 2,
          testCase "Empty List" $ let xs = [] :: [Int] in xs @=? mergeSort xs,
          testCase "Singleton List" $ let xs = ['b'] in xs @=? mergeSort xs
        ]
    ]

puzzleTestGroup :: IO String -> TestTree
puzzleTestGroup rawTestData =
  testGroup
    "Passphrase Validity"
    [
      testGroup "Validity Checks"
      [
        testGroup "Non-Repetitive"
        [
          testCase "Valid Passphrases" $ all isNonRepetitivePassphrase ["aa bb cc dd", "aa bb cc dd aaa", "this is a unique passphrase", "no", ""] @? "Some valid phrases are considered invalid",
          testCase "Invalid Passphrases" $ all (not . isNonRepetitivePassphrase) ["aa bb cc dd aa", "non unique pass pass phrase", "no no no no"] @? "Some invalid phrases are considered valid"
        ],
        testGroup "Non-Anagram"
        [
          testCase "Valid Passphrases" $ all isNonAnagramPassphrase ["abcde fghi", "a ab abc abd abf abj", "iiii oiii ooii oooi oooo", "foo", ""] @? "Some valid phrases are considered invalid",
          testCase "Invalid Passphrases" $ all (not . isNonAnagramPassphrase) ["abcde xyz ecdab", "oiii ioii iioi iiio", "invalid is kinda like valiidn"] @? "Some invalid phrases are considered valid"
        ]
      ],

      testGroup "Puzzle Solutions"
      [
        testCase "Part 1 - Non-Repetitive" $ testData >>= testCountValidPassphrases 383 isNonRepetitivePassphrase,
        testCase "Part 2 - Non-Anagram" $ testData >>= testCountValidPassphrases 265 isNonAnagramPassphrase
      ]
    ]
  where testData = do d <- rawTestData; return (filter (/= "") $ lines d)
