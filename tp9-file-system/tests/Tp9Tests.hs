module Tp9Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Tp9

tests = testGroup "Tests for Tp9"
    [ ex1Tests
    -- , ex2Tests
    -- , ex3Tests
    -- , ex4Tests
    -- , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1" [
    -- [ testGroup "Tests for countVehicles"
    --     [ testCase "grid1 has 2 vehicles" $
    --         countVehicles grid1 @?= 2
    --     , testCase "grid2 has 13 vehicles" $
    --         countVehicles grid2 @?= 13
    --     , testCase "grid3 has 10 vehicles" $
    --         countVehicles grid3 @?= 10
    --     ]
    -- , testGroup "Tests for isCar and isTruck"
    --     [ testCase "cars in grid1" $
    --         L.map (\v -> (v, isCar v)) (vehicles grid1) @?=
    --             [(Vehicle (17,18),True),(Vehicle (13,27),False)]
    --     , testCase "trucks in grid1" $
    --         L.map (\v -> (v, isTruck v)) (vehicles grid1) @?=
    --             [(Vehicle (17,18),False),(Vehicle (13,27),True)]
    --     ]
    -- , testCase "Test for allCells" $
    --     allCells @?= [1, 2, 3, 4, 5, 6,
    --                   8, 9, 10,11,12,13,
    --                   15,16,17,18,19,20,
    --                   22,23,24,25,26,27,
    --                   29,30,31,32,33,34,
    --                   36,37,38,39,40,41]
    ]
