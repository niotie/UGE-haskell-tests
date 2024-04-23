module RAListsTests where

import Test.SmallCheck.Series as SCS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import RALists

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Tuple as T

tests =
    testGroup
        "Tests for RALists"
        [ ex1Tests
        , ex2Tests
        , ex3Tests
        ]

ex1Tests =
    testGroup
        "Tests for exercise 1"
        [ -- e1qaTests,
          e1qbTests
        , e1qcTests
        , e1qdTests
        , e1qeTests
        ]

e1qbTests =
    testGroup
        "Tests for exercise 1 question (b)"
        [ testCase "nullRA nilRA" $
            nullRA nilRA @?= True
        , testCase "nullRA mkRA_ABCDE" $
            nullRA mkRA_ABCDE @?= False
        ]

e1qcTests =
    testGroup
        "Tests for exercise 1 question (c)"
        [ testCase "sizeRA nilRA" $
            sizeRA nilRA @?= 0
        , testCase "sizeRA mkRA_ABCDE" $
            sizeRA mkRA_ABCDE @?= 5
        , testCase "sizeRA mkRA_ABCDEF" $
            sizeRA mkRA_ABCDEF @?= 6
        ]

e1qdTests =
    testGroup
        "Tests for exercise 1 question (d)"
        [ testCase "fromRA nilRA" $
            fromRA nilRA @?= ([] :: [String])
        , testCase "fromRA mkRA_ABCDE" $
            fromRA mkRA_ABCDE @?= "ABCDE"
        , testCase "fromRA mkRA_ABCDEF" $
            fromRA mkRA_ABCDEF @?= "ABCDEF"
        ]

e1qeTests =
    testGroup
        "Tests for exercise 1 question (e)"
        [ testCase "headRA' nilRA" $
            headRA' nilRA @?= (Nothing :: Maybe Char)
        , testCase "headRA' mkRA_ABCDE" $
            headRA' mkRA_ABCDE @?= Just 'A'
        ]

ex2Tests =
    testGroup
        "Tests for exercise 1"
        [ e2qaTests
        , e2qbTests
        , e2qcTests
        , e2qdTests
        , e2qeTests
        ]

e2qaTests =
    testGroup
        "Tests for exercise 2 question (a)"
        []

e2qbTests =
    testGroup
        "Tests for exercise 2 question (b)"
        []

e2qcTests =
    testGroup
        "Tests for exercise 2 question (c)"
        []

e2qdTests =
    testGroup
        "Tests for exercise 2 question (d)"
        []

e2qeTests =
    testGroup
        "Tests for exercise 2 question (e)"
        []

ex3Tests =
    testGroup
        "Tests for exercise 1"
        [ e3qaTests
        , e3qbTests
        , e3qcTests
        , e3qdTests
        , e3qeTests
        ]

e3qaTests =
    testGroup
        "Tests for exercise 3 question (a)"
        []

e3qbTests =
    testGroup
        "Tests for exercise 3 question (b)"
        []

e3qcTests =
    testGroup
        "Tests for exercise 3 question (c)"
        []

e3qdTests =
    testGroup
        "Tests for exercise 3 question (d)"
        []

e3qeTests =
    testGroup
        "Tests for exercise 3 question (e)"
        []
