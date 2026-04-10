{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module RAListsTests where

import Control.Exception (SomeException (SomeException), catch, evaluate)
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

deriving instance (Eq a) => Eq (Tree a)
deriving instance (Eq a) => Eq (Digit a)
deriving instance (Eq a) => Eq (RAList a)

e2qaTests =
  testGroup
    "Tests for exercise 2 question (a)"
    [ testCase "consRA 'A' nilRA" $
        consRA 'A' nilRA @?= RAList{getDigits = [One (Leaf 'A')]}
    , testCase "consRA 'Z' mkRA_ABCDE" $
        consRA 'Z' mkRA_ABCDE @?= RAList{getDigits = [Zero, One (Node 2 (Leaf 'Z') (Leaf 'A')), One (Node 4 (Node 2 (Leaf 'B') (Leaf 'C')) (Node 2 (Leaf 'D') (Leaf 'E')))]}
    , testCase "consRA 'A' . consRA 'B' $ consRA 'C' nilRA" $
        (consRA 'A' . consRA 'B' . consRA 'C') nilRA @?= RAList{getDigits = [One (Leaf 'A'), One (Node 2 (Leaf 'B') (Leaf 'C'))]}
    ]

e2qbTests =
  testGroup
    "Tests for exercise 2 question (b)"
    [ testCase "toRA []" $
        toRA ([] :: [Int]) @?= RAList{getDigits = []}
    , testCase "toRA \"ABCDEF\"" $
        toRA "ABCDEF" @?= RAList{getDigits = [Zero, One (Node 2 (Leaf 'A') (Leaf 'B')), One (Node 4 (Node 2 (Leaf 'C') (Leaf 'D')) (Node 2 (Leaf 'E') (Leaf 'F')))]}
    , testCase "consRA '0' $ toRA \"12345\"" $
        consRA '0' (toRA "12345") @?= RAList{getDigits = [Zero, One (Node 2 (Leaf '0') (Leaf '1')), One (Node 4 (Node 2 (Leaf '2') (Leaf '3')) (Node 2 (Leaf '4') (Leaf '5')))]}
    , localOption (SC.SmallCheckDepth 4)
        . SC.testProperty "fromRA . toRA is invariant"
        $ \l -> (fromRA . toRA) (l :: [Int]) == l
    , QC.testProperty "fromRA . toRA is invariant" $
        \l -> (fromRA . toRA) (l :: [Int]) == l
    ]

e2qcTests =
  testGroup
    "Tests for exercise 2 question (c)"
    [ testCase "unconsRA nilRA" $
        unconsRA (nilRA :: RAList Int) @?= Nothing
    , testCase "unconsRA $ consRA 'A' nilRA" $
        (unconsRA . consRA 'A') nilRA @?= Just ('A', RAList [])
    , testCase "unconsRA mkRA_ABCDE" $
        unconsRA mkRA_ABCDE @?= Just ('A', RAList{getDigits = [Zero, Zero, One (Node 4 (Node 2 (Leaf 'B') (Leaf 'C')) (Node 2 (Leaf 'D') (Leaf 'E')))]})
    , testCase "unconsRA mkRA_ABCDEF" $
        unconsRA mkRA_ABCDEF @?= Just ('A', RAList{getDigits = [One (Leaf 'B'), Zero, One (Node 4 (Node 2 (Leaf 'C') (Leaf 'D')) (Node 2 (Leaf 'E') (Leaf 'F')))]})
    , localOption (SC.SmallCheckDepth 3)
        . SC.testProperty "unconsRA . toRA . (:) behaves as expected"
        $ \a l -> (unconsRA . toRA) (a : l :: [Int]) == Just (a, toRA l)
    , QC.testProperty "unconsRA . toRA . (:) behaves as expected" $
        \a l -> (unconsRA . toRA) (a : l :: [Int]) == Just (a, toRA l)
    ]

e2qdTests =
  testGroup
    "Tests for exercise 2 question (d)"
    [ testCase "headRA nilRA" $
        headRA (nilRA :: RAList Int) @?= Nothing
    , testCase "headRA mkRA_ABCDE" $
        headRA mkRA_ABCDE @?= Just 'A'
    , testCase "tailRA nilRA" $
        tailRA (nilRA :: RAList Int) @?= Nothing
    , testCase "tailRA mkRA_ABCDE" $
        tailRA mkRA_ABCDE @?= Just (RAList{getDigits = [Zero, Zero, One (Node 4 (Node 2 (Leaf 'B') (Leaf 'C')) (Node 2 (Leaf 'D') (Leaf 'E')))]})
    , testCase "let Just ra = tailRA mkRA_ABCDE in consRA 'a' ra" $
        let Just ra = tailRA mkRA_ABCDE in consRA 'a' ra @?= RAList{getDigits = [One (Leaf 'a'), Zero, One (Node 4 (Node 2 (Leaf 'B') (Leaf 'C')) (Node 2 (Leaf 'D') (Leaf 'E')))]}
    , localOption (SC.SmallCheckDepth 3)
        . SC.testProperty "headRA . toRA . (:) behaves as expected"
        $ \a l -> (headRA . toRA) (a : l :: [Int]) == Just a
    , QC.testProperty "headRA . toRA . (:) behaves as expected" $
        \a l -> (headRA . toRA) (a : l :: [Int]) == Just a
    , localOption (SC.SmallCheckDepth 3)
        . SC.testProperty "tailRA . toRA . (:) behaves as expected"
        $ \a l -> (tailRA . toRA) (a : l :: [Int]) == Just (toRA l)
    , QC.testProperty "tailRA . toRA . (:) behaves as expected" $
        \a l -> (tailRA . toRA) (a : l :: [Int]) == Just (toRA l)
    ]

e2qeTests =
  testGroup
    "Tests for exercise 2 question (e)"
    [ localOption (SC.SmallCheckDepth 4)
        . SC.testProperty "fromRA . reverseRA . toRA behaves as expected"
        $ \l -> (fromRA . reverseRA . toRA) (l :: [Int]) == reverse l
    , QC.testProperty "fromRA . reverseRA . toRA behaves as expected" $
        \l -> (fromRA . reverseRA . toRA) (l :: [Int]) == reverse l
    ]

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
    [ testCase "fetchRA (-1) nilRA fails" $
        catch
          (evaluate (fetchRA (-1) nilRA) >> assertFailure "Expected Exception: bad index")
          (\(_ :: SomeException) -> return ())
    , testCase "fetchRA 0 nilRA fails" $
        catch
          (evaluate (fetchRA 0 nilRA) >> assertFailure "Expected Exception: bad index")
          (\(_ :: SomeException) -> return ())
    , testCase "[fetchRA i mkRA_ABCDE | i <- [0..sizeRA mkRA_ABCDE - 1]]" $
        [fetchRA i mkRA_ABCDE | i <- [0 .. sizeRA mkRA_ABCDE - 1]] @?= "ABCDE"
    , testCase "fetchRA (sizeRA mkRA_ABCDE) mkRA_ABCDE fails" $
        catch
          (evaluate (fetchRA (sizeRA mkRA_ABCDE) mkRA_ABCDE) >> assertFailure "Expected Exception: bad index")
          (\(_ :: SomeException) -> return ())
    ]

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
