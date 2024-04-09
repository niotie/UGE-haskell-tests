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
    -- , combineVAExprs1Tests
    -- , mkAExprs1Tests
    -- , searchBestTests
    -- , countdown1Tests
    ]

unmerges1Tests = testGroup "Tests for unmerges1"
    [ HU.testCase "unmerges1 [1,2]" $
        sort (unmerges1 [1,2]) @?= [([1],[2]),([2],[1])]
    , QC.testProperty "all pairs in unmerges1 merge to the initial sorted list" $
        \(ShortSortedList xs) ->
            (QC.forAll . QC.elements) (unmerges1 xs :: [([Int], [Int])]) $
                \(ys, zs) -> collect (show . length $ xs) $ merge ys zs == (xs :: [Int])
    ]

combineVAExprs1Tests = undefined

mkAExprs1Tests :: TestTree
mkAExprs1Tests = undefined

searchBestTests :: TestTree
searchBestTests = undefined

countdown1Tests :: TestTree
countdown1Tests = undefined


