{-# LANGUAGE StandaloneDeriving #-}
module Tp7Tests where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Property as QCP


import Tp7
import Data.List (sort)

newtype ShortSortedList a = ShortSortedList [a]

instance Show a => Show (ShortSortedList a)
    where show (ShortSortedList xs) = show xs

instance (Arbitrary a, Ord a) => Arbitrary (ShortSortedList a) where
    arbitrary = do
        k <- choose (2, 15)
        xs <- vectorOf k arbitrary
        return (ShortSortedList (sort xs))
    shrink (ShortSortedList (x:xs)) =
        [ShortSortedList xs | length xs > 1]

deriving instance Eq BOp
deriving instance Ord BOp
deriving instance Eq AExpr
deriving instance Ord AExpr
deriving instance Eq VAExpr
deriving instance Ord VAExpr

tests = testGroup "Tests for TP 7"
    [ ex1Tests
    , ex2Tests
    -- , ex3Tests
    -- , ex4Tests
    -- , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1 -- Échauffement"
    [ sublistsTests
    , legal1Tests
    , applyTests
    , valueTests
    ]

sublistsTests = testGroup "Tests for sublists"
    [ HU.testCase "sublists []" $
        sublists ([] :: [Int]) @?= ([] :: [[Int]])
    , HU.testCase "sublists [1]" $
        (sort . sublists) [1] @?= [[1]]
    , HU.testCase "sublists [1, 2]" $
        (sort . sublists) [1,2] @?= [[1],[1,2],[2]]
    , HU.testCase "sublists [1, 2, 3]" $
        (sort . sublists) [1,2,3] @?= [[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
    ]

legal1Tests = testGroup "Tests for legal1"
    [ HU.testCase "legal1 Add 12 3" $
        legal1 Add 12 3 @?= True
    , HU.testCase "legal1 Add 3 12" $
        legal1 Add 3 12 @?= True
    , HU.testCase "legal1 Sub 12 3" $
        legal1 Sub 12 3 @?= True
    , HU.testCase "legal1 Sub 3 12" $
        legal1 Sub 3 12 @?= False
    , HU.testCase "legal1 Mul 12 3" $
        legal1 Mul 12 3 @?= True
    , HU.testCase "legal1 Mul 3 12" $
        legal1 Mul 3 12 @?= True
    , HU.testCase "legal1 Div 12 3" $
        legal1 Div 12 3 @?= True
    , HU.testCase "legal1 Div 3 12" $
        legal1 Div 3 12 @?= False
    ]

applyTests = testGroup "Tests for apply"
    [ HU.testCase "apply Add 1 2" $
        apply Add 1 2 @?= 3
    , HU.testCase "apply Sub 5 2" $
        apply Sub 5 2 @?= 3
    , HU.testCase "apply Div 12 4" $
        apply Div 12 4 @?= 3
    , HU.testCase "apply Mul 3 4" $
        apply Mul 3 4 @?= 12
    ]

valueTests = testGroup "Tests for value"
    [ HU.testCase "value (Num 3)" $
        value (Num 3) @?= 3
    , HU.testCase "value (App Add (Num 3) (Num 4))" $
        value (App Add (Num 3) (Num 4)) @?= 7
    , HU.testCase "value (App Add (Num 3) (App Div (Num 24) (Num 6)))" $
        value (App Add (Num 3) (App Div (Num 24) (Num 6))) @?= 7
    , HU.testCase "value (App Mul (App Add (Num 1) (Num 2)) (App Sub (Num 7) (Num 3)))" $
        value (App Mul (App Add (Num 1) (Num 2)) (App Sub (Num 7) (Num 3))) @?= 12
    ]

ex2Tests = testGroup "Tests for exercise 2 -- Le solveur"
    [ unmerges1Tests
    , combineVAExprs1Tests
    , mkAExprs1Tests
    , searchBestTests
    , countdown1Tests
    ]

unmerges1Tests = testGroup "Tests for unmerges1"
    [ HU.testCase "unmerges1 [1,2]" $
        sort (unmerges1 [1,2]) @?= [([1],[2]),([2],[1])]
    , QC.testProperty "all pairs in unmerges1 merge to the initial sorted list" $
        \(ShortSortedList xs) ->
            (QC.forAll . QC.elements) (unmerges1 xs :: [([Int], [Int])]) $
                \(ys, zs) -> collect (show . length $ xs) $ merge ys zs == (xs :: [Int])
    ]

combineVAExprs1Tests = testGroup "Tests for combineVAExprs1"
    [ HU.testCase "combineVAExprs1 (VAExpr (Num 3, 3)) (VAExpr (Num 6, 6))" $
        combineVAExprs1 (VAExpr (Num 3, 3)) (VAExpr (Num 6, 6)) @?= 
            [VAExpr (App Add (Num 3) (Num 6),9),VAExpr (App Mul (Num 3) (Num 6),18)]
    ]

mkAExprs1Tests :: TestTree
mkAExprs1Tests =
  testGroup
    "Tests for mkAExprs1"
    [ HU.testCase "mkAExprs1 [1,2,3]" $
        sort (mkAExprs1 [1, 2, 3])
          @?= [ VAExpr (App Add (Num 1) (App Add (Num 2) (Num 3)), 6),
                VAExpr (App Add (Num 1) (App Add (Num 3) (Num 2)), 6),
                VAExpr (App Add (Num 1) (App Sub (Num 3) (Num 2)), 2),
                VAExpr (App Add (Num 1) (App Mul (Num 2) (Num 3)), 7),
                VAExpr (App Add (Num 1) (App Mul (Num 3) (Num 2)), 7),
                VAExpr (App Add (Num 2) (App Add (Num 1) (Num 3)), 6),
                VAExpr (App Add (Num 2) (App Add (Num 3) (Num 1)), 6),
                VAExpr (App Add (Num 2) (App Sub (Num 3) (Num 1)), 4),
                VAExpr (App Add (Num 2) (App Mul (Num 1) (Num 3)), 5),
                VAExpr (App Add (Num 2) (App Mul (Num 3) (Num 1)), 5),
                VAExpr (App Add (Num 2) (App Div (Num 3) (Num 1)), 5),
                VAExpr (App Add (Num 3) (App Add (Num 1) (Num 2)), 6),
                VAExpr (App Add (Num 3) (App Add (Num 2) (Num 1)), 6),
                VAExpr (App Add (Num 3) (App Sub (Num 2) (Num 1)), 4),
                VAExpr (App Add (Num 3) (App Mul (Num 1) (Num 2)), 5),
                VAExpr (App Add (Num 3) (App Mul (Num 2) (Num 1)), 5),
                VAExpr (App Add (Num 3) (App Div (Num 2) (Num 1)), 5),
                VAExpr (App Add (App Add (Num 1) (Num 2)) (Num 3), 6),
                VAExpr (App Add (App Add (Num 1) (Num 3)) (Num 2), 6),
                VAExpr (App Add (App Add (Num 2) (Num 1)) (Num 3), 6),
                VAExpr (App Add (App Add (Num 2) (Num 3)) (Num 1), 6),
                VAExpr (App Add (App Add (Num 3) (Num 1)) (Num 2), 6),
                VAExpr (App Add (App Add (Num 3) (Num 2)) (Num 1), 6),
                VAExpr (App Add (App Sub (Num 2) (Num 1)) (Num 3), 4),
                VAExpr (App Add (App Sub (Num 3) (Num 1)) (Num 2), 4),
                VAExpr (App Add (App Sub (Num 3) (Num 2)) (Num 1), 2),
                VAExpr (App Add (App Mul (Num 1) (Num 2)) (Num 3), 5),
                VAExpr (App Add (App Mul (Num 1) (Num 3)) (Num 2), 5),
                VAExpr (App Add (App Mul (Num 2) (Num 1)) (Num 3), 5),
                VAExpr (App Add (App Mul (Num 2) (Num 3)) (Num 1), 7),
                VAExpr (App Add (App Mul (Num 3) (Num 1)) (Num 2), 5),
                VAExpr (App Add (App Mul (Num 3) (Num 2)) (Num 1), 7),
                VAExpr (App Add (App Div (Num 2) (Num 1)) (Num 3), 5),
                VAExpr (App Add (App Div (Num 3) (Num 1)) (Num 2), 5),
                VAExpr (App Sub (Num 1) (App Sub (Num 3) (Num 2)), 0),
                VAExpr (App Sub (Num 2) (App Sub (Num 3) (Num 1)), 0),
                VAExpr (App Sub (Num 3) (App Add (Num 1) (Num 2)), 0),
                VAExpr (App Sub (Num 3) (App Add (Num 2) (Num 1)), 0),
                VAExpr (App Sub (Num 3) (App Sub (Num 2) (Num 1)), 2),
                VAExpr (App Sub (Num 3) (App Mul (Num 1) (Num 2)), 1),
                VAExpr (App Sub (Num 3) (App Mul (Num 2) (Num 1)), 1),
                VAExpr (App Sub (Num 3) (App Div (Num 2) (Num 1)), 1),
                VAExpr (App Sub (App Add (Num 1) (Num 2)) (Num 3), 0),
                VAExpr (App Sub (App Add (Num 1) (Num 3)) (Num 2), 2),
                VAExpr (App Sub (App Add (Num 2) (Num 1)) (Num 3), 0),
                VAExpr (App Sub (App Add (Num 2) (Num 3)) (Num 1), 4),
                VAExpr (App Sub (App Add (Num 3) (Num 1)) (Num 2), 2),
                VAExpr (App Sub (App Add (Num 3) (Num 2)) (Num 1), 4),
                VAExpr (App Sub (App Sub (Num 3) (Num 1)) (Num 2), 0),
                VAExpr (App Sub (App Sub (Num 3) (Num 2)) (Num 1), 0),
                VAExpr (App Sub (App Mul (Num 1) (Num 3)) (Num 2), 1),
                VAExpr (App Sub (App Mul (Num 2) (Num 3)) (Num 1), 5),
                VAExpr (App Sub (App Mul (Num 3) (Num 1)) (Num 2), 1),
                VAExpr (App Sub (App Mul (Num 3) (Num 2)) (Num 1), 5),
                VAExpr (App Sub (App Div (Num 3) (Num 1)) (Num 2), 1),
                VAExpr (App Mul (Num 1) (App Add (Num 2) (Num 3)), 5),
                VAExpr (App Mul (Num 1) (App Add (Num 3) (Num 2)), 5),
                VAExpr (App Mul (Num 1) (App Sub (Num 3) (Num 2)), 1),
                VAExpr (App Mul (Num 1) (App Mul (Num 2) (Num 3)), 6),
                VAExpr (App Mul (Num 1) (App Mul (Num 3) (Num 2)), 6),
                VAExpr (App Mul (Num 2) (App Add (Num 1) (Num 3)), 8),
                VAExpr (App Mul (Num 2) (App Add (Num 3) (Num 1)), 8),
                VAExpr (App Mul (Num 2) (App Sub (Num 3) (Num 1)), 4),
                VAExpr (App Mul (Num 2) (App Mul (Num 1) (Num 3)), 6),
                VAExpr (App Mul (Num 2) (App Mul (Num 3) (Num 1)), 6),
                VAExpr (App Mul (Num 2) (App Div (Num 3) (Num 1)), 6),
                VAExpr (App Mul (Num 3) (App Add (Num 1) (Num 2)), 9),
                VAExpr (App Mul (Num 3) (App Add (Num 2) (Num 1)), 9),
                VAExpr (App Mul (Num 3) (App Sub (Num 2) (Num 1)), 3),
                VAExpr (App Mul (Num 3) (App Mul (Num 1) (Num 2)), 6),
                VAExpr (App Mul (Num 3) (App Mul (Num 2) (Num 1)), 6),
                VAExpr (App Mul (Num 3) (App Div (Num 2) (Num 1)), 6),
                VAExpr (App Mul (App Add (Num 1) (Num 2)) (Num 3), 9),
                VAExpr (App Mul (App Add (Num 1) (Num 3)) (Num 2), 8),
                VAExpr (App Mul (App Add (Num 2) (Num 1)) (Num 3), 9),
                VAExpr (App Mul (App Add (Num 2) (Num 3)) (Num 1), 5),
                VAExpr (App Mul (App Add (Num 3) (Num 1)) (Num 2), 8),
                VAExpr (App Mul (App Add (Num 3) (Num 2)) (Num 1), 5),
                VAExpr (App Mul (App Sub (Num 2) (Num 1)) (Num 3), 3),
                VAExpr (App Mul (App Sub (Num 3) (Num 1)) (Num 2), 4),
                VAExpr (App Mul (App Sub (Num 3) (Num 2)) (Num 1), 1),
                VAExpr (App Mul (App Mul (Num 1) (Num 2)) (Num 3), 6),
                VAExpr (App Mul (App Mul (Num 1) (Num 3)) (Num 2), 6),
                VAExpr (App Mul (App Mul (Num 2) (Num 1)) (Num 3), 6),
                VAExpr (App Mul (App Mul (Num 2) (Num 3)) (Num 1), 6),
                VAExpr (App Mul (App Mul (Num 3) (Num 1)) (Num 2), 6),
                VAExpr (App Mul (App Mul (Num 3) (Num 2)) (Num 1), 6),
                VAExpr (App Mul (App Div (Num 2) (Num 1)) (Num 3), 6),
                VAExpr (App Mul (App Div (Num 3) (Num 1)) (Num 2), 6),
                VAExpr (App Div (Num 1) (App Sub (Num 3) (Num 2)), 1),
                VAExpr (App Div (Num 2) (App Sub (Num 3) (Num 1)), 1),
                VAExpr (App Div (Num 3) (App Add (Num 1) (Num 2)), 1),
                VAExpr (App Div (Num 3) (App Add (Num 2) (Num 1)), 1),
                VAExpr (App Div (Num 3) (App Sub (Num 2) (Num 1)), 3),
                VAExpr (App Div (App Add (Num 1) (Num 2)) (Num 3), 1),
                VAExpr (App Div (App Add (Num 1) (Num 3)) (Num 2), 2),
                VAExpr (App Div (App Add (Num 2) (Num 1)) (Num 3), 1),
                VAExpr (App Div (App Add (Num 2) (Num 3)) (Num 1), 5),
                VAExpr (App Div (App Add (Num 3) (Num 1)) (Num 2), 2),
                VAExpr (App Div (App Add (Num 3) (Num 2)) (Num 1), 5),
                VAExpr (App Div (App Sub (Num 3) (Num 1)) (Num 2), 1),
                VAExpr (App Div (App Sub (Num 3) (Num 2)) (Num 1), 1),
                VAExpr (App Div (App Mul (Num 2) (Num 3)) (Num 1), 6),
                VAExpr (App Div (App Mul (Num 3) (Num 2)) (Num 1), 6)
              ]
    ]

searchBestTests :: TestTree
searchBestTests =
  testGroup
    "Tests for searchBest"
    [ HU.testCase "searchBest 9 $ mkAExprs1 [1,2,3]" $
        let VAExpr (e, v) = searchBest 9 (mkAExprs1 [1, 2, 3])
         in v @?= 9,
      HU.testCase "searchBest 3 $ mkAExprs1 [1,2,3]" $
        let VAExpr (e, v) = searchBest 3 (mkAExprs1 [1, 2, 3])
         in v @?= 3,
      HU.testCase "searchBest 10 $ mkAExprs1 [1,2,3]" $
        let VAExpr (e, v) = searchBest 10 (mkAExprs1 [1, 2, 3])
         in v @?= 9
    ]

countdown1Tests :: TestTree
countdown1Tests =
  testGroup
    "Tests for countdown1"
    [ HU.testCase "countdown1 463 [1,2,3,4,5,25]" $
        let VAExpr (e, v) = countdown1 463 [1, 2, 3, 4, 5, 25]
         in v @?= 463,
      HU.testCase "countdown1 463 [1,4,7,8,9,9]" $
        let VAExpr (e, v) = countdown1 463 [1, 4, 7, 8, 9, 9]
         in v @?= 463,
      HU.testCase "countdown1 463 [1,2,3,4,5,6]" $
        let VAExpr (e, v) = countdown1 463 [1, 2, 3, 4, 5, 6]
         in v @?= 462
    ]