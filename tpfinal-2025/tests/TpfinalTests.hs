module TpfinalTests where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
-- import Test.SmallCheck.Series as SCS

import Data.Map as M
import Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Tpfinal

tests =
  testGroup
    "Tests for Tpfinal"
    [ ex1Tests,
      ex2Tests,
      ex3Tests,
      ex4Tests
    ]

countPeaksTest =
  testGroup
    "Tests for countPeaks"
    [ testCase "countPeaks ([] :: [Int]) = 0" $
        countPeaks ([] :: [Int]) @?= 0,
      testCase "countPeaks [1] = 0" $
        countPeaks [1] @?= 0,
      testCase "countPeaks [1,2] = 0" $
        countPeaks [1, 2] @?= 0,
      testCase "countPeaks [1,2,3] = 0" $
        countPeaks [1, 2, 3] @?= 0,
      testCase "countPeaks [1,3,2] = 1" $
        countPeaks [1, 3, 2] @?= 1,
      testCase "countPeaks [1,5,2,3,4,5,5] = 1" $
        countPeaks [1, 5, 2, 3, 4, 5, 5] @?= 1,
      testCase "countPeaks [1,5,2,3,4,2,5] = 2" $
        countPeaks [1, 5, 2, 3, 4, 2, 5] @?= 2
    ]

runsTest =
  testGroup
    "Tests for runs"
    [ testCase "runs [] = []" $
        runs ([] :: [Int]) @?= [],
      testCase "runs [1] = [[1]]" $
        runs [1] @?= [[1]],
      testCase "runs [1,2,3,4,5] = [[1,2,3,4,5]]" $
        runs [1, 2, 3, 4, 5] @?= [[1, 2, 3, 4, 5]],
      testCase "runs [1,4,3,5,7,5,4,6,8,3,5] = [[1,4],[3,5,7],...]" $
        runs [1, 4, 3, 5, 7, 5, 4, 6, 8, 3, 5] @?= [[1, 4], [3, 5, 7], [5], [4, 6, 8], [3, 5]]
    ]

flattenTests =
  testGroup
    "Tests for flatten"
    [ testCase "semiPalindrome \"\" = True" $
        semiPalindrome "" @?= True,
      testCase "semiPalindrome \"aabb\" = True" $
        semiPalindrome "aabb" @?= True,
      testCase "semiPalindrome \"aabba\" = True" $
        semiPalindrome "aabba" @?= True,
      testCase "semiPalindrome \"aabbab\" = False" $
        semiPalindrome "aabbab" @?= False
    ]

semiPalidromeTests =
  testGroup
    "Tests for semiPalidrome"
    [ testCase "flatten [] = []" $
        flatten ([] :: [[Int]]) @?= [],
      testCase "flatten [[1,2,3]] = [1,2,3]" $
        flatten [[1, 2, 3]] @?= [1, 2, 3],
      testCase "flatten [[1,2,3],[4],[5,6],[7,8,9]] = [1,2,...,9]" $
        flatten [[1, 2, 3], [4], [5, 6], [7, 8, 9]] @?= [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ]

interleavingsTests =
  testGroup
    "Tests for interleavings"
    [ testCase "interleavings ['a','b','c'] [] = [\"abc\"]" $
        interleavings ['a', 'b', 'c'] [] @?= ["abc"],
      testCase "interleavings [] ['A','B'] = [\"AB\"]" $
        interleavings [] ['A', 'B'] @?= ["AB"],
      testCase "interleavings ['a','b','c'] ['A','B'] = ..." $
        interleavings ['a', 'b', 'c'] ['A', 'B'] @?= ["abcAB", "abAcB", "abABc", "aAbcB", "aAbBc", "aABbc", "AabcB", "AabBc", "AaBbc", "ABabc"]
    ]

ex1Tests =
  testGroup
    "Tests for exercise 1"
    [ countPeaksTest,
      runsTest,
      flattenTests,
      semiPalidromeTests,
      interleavingsTests
    ]

ex2Tests =
  testGroup
    "Tests for exercise 2"
    []

addMyMapTests =
  testGroup
    "Tests for addMyMapTests"
    [ testCase "addMyMap 1 myMap1" $
        addMyMap 1 myMap1 @?= M.fromList [(1, S.fromList [1, 2]), (2, S.fromList [1, 3]), (3, S.fromList [1, 2, 3, 4])],
      testCase "addMyMap 1 myMap2" $
        addMyMap 1 myMap2 @?= M.fromList [(2, S.fromList [1, 2]), (3, S.fromList [1, 2, 3]), (4, S.fromList [1])],
      testCase "addMyMap 1 myMap3" $
        addMyMap 1 myMap3 @?= M.fromList [(1, S.fromList [1, 2]), (3, S.fromList [1, 3]), (4, S.fromList [1, 3, 5])]
    ]

onlySingletonMyMapTests =
  testGroup
    "Tests for onlySingletonMyMapTests"
    [ testCase "onlySingletonMyMap myMap1" $
        onlySingletonMyMap myMap1
          @?= M.fromList [(1, S.fromList [2]), (2, S.fromList [3])],
      testCase "onlySingletonMyMap myMap2" $
        onlySingletonMyMap myMap2
          @?= M.fromList [(2, S.fromList [2]), (4, S.fromList [1])],
      testCase "onlySingletonMyMap myMap3" $
        onlySingletonMyMap myMap3
          @?= M.fromList [(1, S.fromList [2]), (3, S.fromList [3])]
    ]

fusionMyMapTests =
  testGroup
    "Tests for fusionMyMapTests"
    []

lookupMyMapTests =
  testGroup
    "Tests for lookupMyMapTests"
    []

reverseMyMapTests =
  testGroup
    "Tests for reverseMyMapTests"
    []

ex3Tests =
  testGroup
    "Tests for exercise 3"
    [ addMyMapTests,
      onlySingletonMyMapTests,
      fusionMyMapTests,
      lookupMyMapTests,
      reverseMyMapTests
    ]

incRLERunTests =
  testGroup
    "Tests for incRLERun"
    []

decRLERunTests =
  testGroup
    "Tests for decRLERun"
    []

consRLEStringTests =
  testGroup
    "Tests for consRLEString"
    []

fromListRLEStringTests =
  testGroup
    "Tests for fromListRLEString"
    []

headRLEStringTests =
  testGroup
    "Tests for headRLEString"
    []

tailRLEStringTests =
  testGroup
    "Tests for tailRLEString"
    []

initRLEStringTests =
  testGroup
    "Tests for initRLEString"
    []

concatRLEStringTests =
  testGroup
    "Tests for (@++@)"
    []

mapRLEStringTests =
  testGroup
    "Tests for mapRLEString"
    []

palindromeRLEStringTests =
  testGroup
    "Tests for palindromeRLEString"
    []

takeRLEStringTests =
  testGroup
    "Tests for takeRLEString"
    []

dropRLEStringTests =
  testGroup
    "Tests for dropRLEString"
    []

splitAtRLEStringTests =
  testGroup
    "Tests for splitAtRLEString"
    []

ex4Tests =
  testGroup
    "Tests for exercise 4"
    [ incRLERunTests,
      decRLERunTests,
      consRLEStringTests,
      fromListRLEStringTests,
      headRLEStringTests,
      tailRLEStringTests,
      initRLEStringTests,
      concatRLEStringTests,
      mapRLEStringTests,
      palindromeRLEStringTests,
      takeRLEStringTests,
      dropRLEStringTests,
      splitAtRLEStringTests
    ]