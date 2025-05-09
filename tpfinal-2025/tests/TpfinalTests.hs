module TpfinalTests where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
-- import Test.SmallCheck.Series as SCS

import Tpfinal

tests = testGroup "Tests for Tpfinal"
    [ ex1Tests
    ]

countPeaksTest = testGroup "Tests for countPeaks" 
    [ testCase "countPeaks ([] :: [Int]) = 0" $
        countPeaks ([] :: [Int]) @?= 0
    , testCase "countPeaks [1] = 0" $
        countPeaks [1] @?= 0
    , testCase "countPeaks [1,2] = 0" $
        countPeaks [1,2] @?= 0
    , testCase "countPeaks [1,2,3] = 0" $
        countPeaks [1,2,3] @?= 0
    , testCase "countPeaks [1,3,2] = 1" $
        countPeaks [1,3,2] @?= 1
    , testCase "countPeaks [1,5,2,3,4,5,5] = 1" $
        countPeaks [1,5,2,3,4,5,5] @?= 1
    , testCase "countPeaks [1,5,2,3,4,2,5] = 2" $
        countPeaks [1,5,2,3,4,2,5] @?= 2
    ]

runsTest = testGroup "Tests for runs" 
    [ testCase "runs [] = []" $
        runs ([] :: [Int]) @?= []
    , testCase "runs [1] = [[1]]" $
        runs [1] @?= [[1]]
    , testCase "runs [1,2,3,4,5] = [[1,2,3,4,5]]" $
        runs [1,2,3,4,5] @?= [[1,2,3,4,5]]
    , testCase "runs [1,4,3,5,7,5,4,6,8,3,5] = [[1,4],[3,5,7],...]" $
        runs [1,4,3,5,7,5,4,6,8,3,5] @?= [[1,4],[3,5,7],[5],[4,6,8],[3,5]]
    ]

flattenTests = testGroup "Tests for flatten"
    [ testCase "semiPalindrome \"\" = True" $
        semiPalindrome "" @?= True
    , testCase "semiPalindrome \"aabb\" = True" $
        semiPalindrome "aabb" @?= True
    , testCase "semiPalindrome \"aabba\" = True" $
        semiPalindrome "aabba" @?= True
    , testCase "semiPalindrome \"aabbab\" = False" $
        semiPalindrome "aabbab" @?= False
    ]

semiPalidromeTests = testGroup "Tests for semiPalidrome"
    [ testCase "flatten [] = []" $
        flatten ([] :: [[Int]]) @?= []
    , testCase "flatten [[1,2,3]] = [1,2,3]" $
        flatten [[1,2,3]] @?= [1,2,3]
    , testCase "flatten [[1,2,3],[4],[5,6],[7,8,9]] = [1,2,...,9]" $
        flatten [[1,2,3],[4],[5,6],[7,8,9]] @?= [1,2,3,4,5,6,7,8,9]
    ]

shuffleTests = testGroup "Tests for shuffle"
    [ testCase "shuffles ['a','b','c'] [] = [\"abc\"]" $
        shuffles ['a','b','c'] [] @?= ["abc"]
    , testCase "shuffles [] ['A','B'] = [\"AB\"]" $
        shuffles [] ['A','B'] @?= ["AB"]
    , testCase "shuffles ['a','b','c'] ['A','B'] = ..." $
    shuffles ['a','b','c'] ['A','B'] @?= ["abcAB","abAcB","abABc","aAbcB","aAbBc","aABbc","AabcB","AabBc","AaBbc","ABabc"]
    ]

ex1Tests = testGroup "Tests for exercise 1" 
    [ countPeaksTest
    , runsTest
    , flattenTests
    , semiPalidromeTests
    , shuffleTests
    ]
