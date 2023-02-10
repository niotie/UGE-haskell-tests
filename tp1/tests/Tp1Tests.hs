module Tp1Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Data.List (minimum, maximum)

import Tp1

tests = testGroup "Tests for exercise 4"
    [ myAverageTests -- (a)
    , myMin2Tests
    , myMax2Tests
    , myMin3Tests
    , myMax3Tests
    , myAddTests
    , myMulTests
    , myFactTests -- (e) et (f)
    , myGCDEuclidTests
    ]

myAverageTests = testGroup "Unit tests for myAverage"
    [ testCase "myAverage 1 1 1" $
        myAverage 1 1 1    @?= 1
    , testCase "myAverage -1 1 1" $
        myAverage (-1) 1 1 @?= 1/3
    , testCase "myAverage 1 2 3" $
        myAverage 1 2 3    @?= 2
    ]

myMin2Tests = testGroup "Property tests for myMin2"
    [ SC.testProperty "myMin2 equals min (SmallCheck)" $
        \a b -> myMin2 a b == min a b
    , QC.testProperty "myMin2 equals min (QuickCheck)" $
        \a b -> myMin2 a b == min a b
    ]

myMax2Tests = testGroup "Property tests for myMax2"
    [ SC.testProperty "myMax2 equals max (SmallCheck)" $
        \a b -> myMax2 a b == max a b
    , QC.testProperty "myMax2 equals max (QuickCheck)" $
        \a b -> myMax2 a b == max a b
    ]

myMin3Tests = testGroup "Property tests for myMin3"
    [ SC.testProperty "myMin3 equals minimum (SmallCheck)" $
        \a b c -> myMin3 a b c == minimum [a, b, c]
    , QC.testProperty "myMin3 equals minimum (QuickCheck)" $
        \a b c -> myMin3 a b c == minimum [a, b, c]
    ]

myMax3Tests = testGroup "Property tests for myMax3"
    [ SC.testProperty "myMax3 equals maximum (SmallCheck)" $
        \a b c -> myMax3 a b c == maximum [a, b, c]
    , QC.testProperty "myMax3 equals maximum (QuickCheck)" $
        \a b c -> myMax3 a b c == maximum [a, b, c]
    ]

myAddTests = testGroup "Property tests for myAdd"
    [ SC.testProperty "myAdd equals (+) (SmallCheck)" $
        \a b -> myAdd a b == a + b
    , QC.testProperty "myAdd equals (+) (QuickCheck)" $
        \a b -> myAdd a b == a + b
    ]

myMulTests = testGroup "Property tests for myMul"
    [ SC.testProperty "myMul equals (*) (SmallCheck)" $
        \a b -> myMul a b == a * b
    , QC.testProperty "myMul equals (*) (QuickCheck)" $
        \a b -> myMul a b == a * b
    ]

myFactTests = testGroup "Unit tests for myFact"
    [ testCase "myFact 0" $
        myFact 0 @?= 1
    , testCase "myFact 1" $
        myFact 1 @?= 1
    , testCase "myFact 2" $
        myFact 2 @?= 2
    , testCase "myFact 5" $
        myFact 5 @?= 120
    ]

myGCDEuclidTests = testGroup "Property tests for myGCDEuclid"
    [ SC.testProperty "myGCDEuclid equals gcd (SmallCheck)" $
        \a b -> myGCDEuclid a b == gcd a b
    , QC.testProperty "myGCDEuclid equals gcd (QuickCheck)" $
        \a b -> myGCDEuclid a b == gcd a b
    ]