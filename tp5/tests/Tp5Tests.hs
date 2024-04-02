module Tp5Tests where

import Control.Monad (liftM, liftM3)

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Test.QuickCheck.Test as QCT
import Test.QuickCheck.Property as QCP

import Tp5
import Data.List (sort)
import GHC.Arr (elems)

data SizedBSTree a = SizedBSTree {size :: Int, tree :: BSTree a}
    deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (SizedBSTree a) where
    arbitrary = do
        n <- arbitrary
        t <- treeWithNodeCount (abs n)
        return SizedBSTree {size = abs n, tree = t}

instance (Arbitrary a, Ord a) => Arbitrary (BSTree a) where
    arbitrary = sized treeWithNodeCount

    shrink Empty = []
    shrink (Node lt x rt) =
        [lt, rt] ++ [Node lt' x rt' | lt' <- shrink lt, rt' <- shrink rt]

treeWithNodeCount n = do
    xs <- sortedVector n
    treeWithNodes xs

treeWithNodes [] = return Empty
treeWithNodes xs = oneof [
        liftM3 Node lt (return x) rt |
        (ls, x, rs) <- splits xs,
        let lt = treeWithNodes ls,
        let rt = treeWithNodes rs
    ]

sortedVector n = sort `fmap` vectorOf n arbitrary

sortedList = getSorted `fmap` arbitrary

splits [x] = [([], x, [])]
splits (x:xs) = ([], x, xs) : map (aux x) (splits xs)
    where aux x (ys, y, ys') = (x:ys, y, ys')
splits [] = error "can't split empty list"

-- treeWithNodeCount :: Int -> Gen (BSTree Int)
-- treeWithNodeCount = aux 1
--     where
--         aux l g | l > g = return Empty
--                 | otherwise = oneof [
--                     liftM3 Node lt (return k) rt
--                     | k <- [l..g],
--                       let lt = aux l (pred k),
--                       let rt = aux (succ k) g]

-- Create a strictly binary search tree with n leaves
treeWithLeafCount n = aux 1 (2 * n - 1)
    where
        aux l g | l > g = return Empty
                | l == g = return $ Node Empty l Empty
                | otherwise = oneof [
                    liftM3 Node lt (return k) rt
                    | k <- [l+1,l+3..g-1],
                      let lt = aux l (k-1),
                      let rt = aux (k+1) g]

treeWithHeight h = aux h 1 (2^h-1)
    where
        aux h l g | h == 0 = return Empty
                  | h == 0 = return $ Node Empty ((l + g) `div` 2) Empty
                  | otherwise = oneof $
                        [liftM3 Node lt (return r) rt |
                            let r = (l + g) `div` 2,
                            k <- [0 .. h - 1],
                            (p, q) <- [(k, h-1), (h-1, k)],
                            let lt = aux p l (pred r),
                            let rt = aux q (succ r) g]

tests = testGroup "Tests for TP 5"
    [ ex1Tests
    , ex2Tests
    -- , ex3Tests
    -- , ex4Tests
    -- , ex5Tests
    -- , ex6Tests
    -- , ex7Tests
    ]

ex1Tests = testGroup "Tests for exercise 1 -- Échauffement"
    [ mkExampleBSTreeTest
    , countNodesTest
    , countNodesTest'
    , countLeavesTest
    , heightTest
    , elemTestPos
    , elemTestNeg
    ]

mkExampleBSTreeTest = HU.testCase "mkTreeExample is properly defined" $
        mkExampleBSTree @?= Node
            (Node (Node (Node Empty 1 Empty)
                        2
                        (Node Empty 3 Empty))
                  4
                  (Node Empty 5 Empty))
            6
            (Node (Node Empty 7 Empty)
                  8
                  (Node Empty 9 Empty))

countNodesTest = QC.testProperty "count nodes of known-sized trees" $ do
    n <- choose (0, 20)
    t <- treeWithNodeCount n :: Gen (BSTree Int)
    let n' = (countNodesBSTree t :: Int)
    return $ if n' == n
             then succeeded
             else failed { QCP.reason = show t ++ "\nhas " ++ show n ++ " nodes but you found " ++ show n' }

countNodesTest' = QC.testProperty "count nodes of known-sized trees" $
    \st -> countNodesBSTree (tree st) == size (st :: SizedBSTree Int)

countLeavesTest = QC.testProperty "count leaves of known-sized trees" $ do
    n <- choose (0, 20) :: Gen Int
    t <- treeWithLeafCount n
    let n' = (countLeavesBSTree t :: Int)
    return $ if n' == n
             then succeeded
             else failed { QCP.reason = show t ++ "\nhas " ++ show n ++ " leaves but you found " ++ show n' }

heightTest = QC.testProperty "measure height of known-height trees" $ do
    h <- choose (0, 10) :: Gen Int
    t <- treeWithHeight h
    let h' = (heightBSTree t :: Int)
    return $ if h' == h
             then QCP.succeeded
             else QCP.failed { QCP.reason = show t ++ "\nhas height " ++ show h ++ " but you found " ++ show h' }

elemTestPos = QC.testProperty "search elements in known BST (present element)" $ do
    xs <- sortedList `suchThat` (not . null) :: Gen [Int]
    t <- treeWithNodes xs
    x <- elements xs
    return $ if x `elemBSTree` t
             then QCP.succeeded
             else QCP.failed { QCP.reason = show x ++ " should have been found in " ++ show t }

elemTestNeg = QC.testProperty "search element in known BST (absent element)" $ do
    xs <- sortedList `suchThat` (not . null) :: Gen [Int]
    x <- arbitrary `suchThat` (not . (`elem` xs))
    t <- treeWithNodes xs
    return $ if not $ x `elemBSTree` t
             then QCP.succeeded
             else QCP.failed { QCP.reason = show x ++ " should not have been found in " ++ show t }

ex2Tests = testGroup "Tests for exercise 2 -- Parcours"
    [ inOrderTest
    ]

inOrderTest = QC.testProperty "values in infix visit are sorted" $
    \t -> let xs = (inOrderVisitBSTree (t :: BSTree Int) :: [Int]) in 
        if sort xs == xs
        then QCP.succeeded
        else QCP.failed { QCP.reason = "infix visit of " ++ show t ++ "returned " ++ show xs ++ ", which is not sorted" }
