module Tp8Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Tp8

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

tests = testGroup "Tests for Tp8"
    [ ex1Tests
    , ex2Tests
    , ex3Tests
    -- , ex4Tests
    -- , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1"
    [ testGroup "Tests for countVehicles"
        [ testCase "grid1 has 2 vehicles" $
            countVehicles grid1 @?= 2
        , testCase "grid2 has 13 vehicles" $
            countVehicles grid2 @?= 13
        , testCase "grid3 has 10 vehicles" $
            countVehicles grid3 @?= 10
        ]
    , testGroup "Tests for isCar and isTruck"
        [ testCase "cars in grid1" $
            L.map (\v -> (v, isCar v)) (vehicles grid1) @?=
                [(Vehicle (17,18),True),(Vehicle (13,27),False)]
        , testCase "trucks in grid1" $
            L.map (\v -> (v, isTruck v)) (vehicles grid1) @?=
                [(Vehicle (17,18),False),(Vehicle (13,27),True)]
        ]
    , testCase "Test for allCells" $
        allCells @?= [1, 2, 3, 4, 5, 6,
                      8, 9, 10,11,12,13,
                      15,16,17,18,19,20,
                      22,23,24,25,26,27,
                      29,30,31,32,33,34,
                      36,37,38,39,40,41]
    , testGroup "Tests for fillCells"
        [ testCase "fillCells in grid1" $
            L.map fillCells (vehicles grid1) @?=
                [[17,18],[13,20,27]]
        , testCase "fillCells in grid2" $
            L.map fillCells (vehicles grid2) @?= 
                [[17,18],[13,20,27],[1,8,15],[2,9],[3,10],[4,11],[5,6],
                 [12,19],[24,25,26],[31,38],[33,34],[36,37],[40,41]]
        , testCase "fillCells in grid3" $
            L.map fillCells (vehicles grid3) @?= 
                [[17,18],[4,11],[6,13,20],[27,34],[36,37],[1,8,15],
                 [22,23],[26,33],[24,31],[38,39]]
        ]
    , testGroup "Tests for occupiedCells"
        [ testCase "occupied cells in grid1" $
            L.sort (occupiedCells grid1) @?=
                [13,17,18,20,27]
        , testCase "occupied cells in grid2" $
            L.sort (occupiedCells grid2) @?= 
                [1,2,3,4,5,6,8,9,10,11,12,13,15,17,18,19,20,24,25,26,
                 27,31,33,34,36,37,38,40,41]
        , testCase "occupied cells in grid3" $
            L.sort (occupiedCells grid3) @?= 
                [1,4,6,8,11,13,15,17,18,20,22,23,24,26,27,31,33,34,36,
                 37,38,39]
        ]
    , testGroup "Tests for freeCells"
        [ testCase "free cells in grid1" $
            L.sort (freeCells grid1) @?=
                [1,2,3,4,5,6,8,9,10,11,12,15,16,19,22,23,24,25,26,29,
                 30,31,32,33,34,36,37,38,39,40,41]
        , testCase "free cells in grid2" $
            L.sort (freeCells grid2) @?= 
                [16,22,23,29,30,32,39]
        , testCase "free cells in grid3" $
            L.sort (freeCells grid3) @?= 
                [2,3,5,9,10,12,16,19,25,29,30,32,40,41]
        ]
    ]

ex2Tests = testGroup "Tests for exercise 1"
    [ testGroup "Tests for adjCells"
        [ testCase "adjacent cells in grid1" $
            L.map (\v -> (v, adjCells v)) (vehicles grid1) @?= 
                [(Vehicle (17,18),[16,19]),(Vehicle (13,27),[6,34])]
        , testCase "adjacent cells in grid2" $
            L.map (\v -> (v, adjCells v)) (vehicles grid2) @?=
                [(Vehicle (17,18),[16,19]),(Vehicle (13,27),[6,34]),
                 (Vehicle (1,15),[22]),(Vehicle (2,9),[16]),
                 (Vehicle (3,10),[17]),(Vehicle (4,11),[18]),
                 (Vehicle (5,6),[4]),(Vehicle (12,19),[5,26]),
                 (Vehicle (24,26),[23,27]),(Vehicle (31,38),[24]),
                 (Vehicle (33,34),[32]),(Vehicle (36,37),[38]),
                 (Vehicle (40,41),[39])]
        , testCase "adjacent cells in grid3" $
            L.map (\v -> (v, adjCells v)) (vehicles grid3) @?=
                [(Vehicle (17,18),[16,19]),(Vehicle (4,11),[18]),
                 (Vehicle (6,20),[27]),(Vehicle (27,34),[20,41]),
                 (Vehicle (36,37),[38]),(Vehicle (1,15),[22]),
                 (Vehicle (22,23),[24]),(Vehicle (26,33),[19,40]),
                 (Vehicle (24,31),[17,38]),(Vehicle (38,39),[37,40])]
        ]
    , testGroup "Tests for legalMoves"
        [ testCase "legal moves in grid1" $
            legalMoves grid1 @?= 
                [Move (0,16),Move (0,19),Move (1,6),Move (1,34)]
        , testCase "legal moves in grid2" $
            legalMoves grid2 @?=
                [Move (0,16),Move (2,22),Move (3,16),Move (8,23),
                 Move (10,32),Move (12,39)]
        , testCase "legal moves in grid3" $
            legalMoves grid3 @?=
                [Move (0,16),Move (0,19),Move (3,41),Move (7,19),
                 Move (7,40),Move (9,40)]
        ]
    , testGroup "Tests for moveVehicleTowards"
        [ testCase "test 1" $
            moveVehicleTowards (Vehicle (3,4)) 5 @?= Vehicle (4,5)
        , testCase "test 2" $
            moveVehicleTowards (Vehicle (3,4)) 2 @?= Vehicle (2,3)
        , testCase "test 3" $
            moveVehicleTowards (Vehicle (10,24)) 31 @?= Vehicle (17,31)
        , testCase "test 4" $
            moveVehicleTowards (Vehicle (10,24)) 3 @?= Vehicle (3,17)
        ]
    , testGroup "Tests for move"
        [ testCase "test 1" $
            let m = Move (1, 34) in move grid1 m 
            @?= Grid {vehicles = [Vehicle (17,18),Vehicle (20,34)]}
        , testCase "test 2" $
            let m1 = Move (1, 34); m2 = Move (1,41) 
            in move (move grid1 m1) m2 
            @?= Grid {vehicles = [Vehicle (17,18),Vehicle (27,41)]}
        , testCase "test 3" $
            F.foldl move grid1 (L.map Move [(1,34),(1,41),(0,19),(0,20)])
            @?= Grid {vehicles = [Vehicle (19,20),Vehicle (27,41)]}
        ]
    ]

ex3Tests = testGroup "Tests for exercise 3"
    [ testGroup "Tests for isSolved"
        [ testCase "test on grid1, grid2, grid3" $
            L.map isSolved [grid1,grid2,grid3] 
            @?= [False,False,False]
        , testCase "test on grid1 after a few moves" $
            (isSolved . F.foldl move grid1 . L.map Move) 
                [(1,34),(1,41),(0,19),(0,20)] 
            @?= True
        ]
    , testGroup "Tests for succPaths"
        [ testCase "test 1" $
            succPaths (Path ([], grid1)) @?=
                [Path ([Move (0,16)],
                       Grid {vehicles = [Vehicle (16,17),Vehicle (13,27)]}),
                 Path ([Move (0,19)],
                       Grid {vehicles = [Vehicle (18,19),Vehicle (13,27)]}),
                 Path ([Move (1,6)],
                       Grid {vehicles = [Vehicle (17,18),Vehicle (6,20)]}),
                 Path ([Move (1,34)],
                       Grid {vehicles = [Vehicle (17,18),Vehicle (20,34)]})]
        , testCase "test 2" $
            L.length (succPaths (Path ([], grid2))) @?= 6
        , testCase "test 3" $
            L.head (succPaths (Path ([], grid2))) @?=
                Path ([Move (0,16)],
                      Grid {vehicles = [Vehicle (16,17),Vehicle (13,27),
                                        Vehicle (1,15),Vehicle (2,9),
                                        Vehicle (3,10),Vehicle (4,11),
                                        Vehicle (5,6),Vehicle (12,19),
                                        Vehicle (24,26),Vehicle (31,38),
                                        Vehicle (33,34),Vehicle (36,37),
                                        Vehicle (40,41)]})
        , testCase "test 4" $
            succPaths (Path ([Move (0,16),Move (1,34)], grid1)) @?=
                [Path ([Move (0,16),Move (1,34),Move (0,16)],
                       Grid {vehicles = [Vehicle (16,17),Vehicle (13,27)]}),
                 Path ([Move (0,16),Move (1,34),Move (0,19)],
                       Grid {vehicles = [Vehicle (18,19),Vehicle (13,27)]}),
                 Path ([Move (0,16),Move (1,34),Move (1,6)],
                       Grid {vehicles = [Vehicle (17,18),Vehicle (6,20)]}),
                 Path ([Move (0,16),Move (1,34),Move (1,34)],
                       Grid {vehicles = [Vehicle (17,18),Vehicle (20,34)]})]
        ]
    , testGroup "Tests for bfsSolve"
        [ testCase "test on grid1" $
            bfsSolve grid1 @?=
                Just [Move (0,19),Move (1,34),Move (1,41),Move (0,20)]
        , testCase "test on grid2" $
            bfsSolve grid2 @?=
                Just [Move (2,22),Move (2,29),Move (3,16),Move (3,23),
                      Move (3,30),Move (0,16),Move (5,18),Move (6,4),
                      Move (1,6),Move (8,27),Move (9,24),Move (10,32),
                      Move (11,38),Move (2,36),Move (0,15),Move (4,17),
                      Move (6,3),Move (6,2),Move (5,4),Move (6,1),
                      Move (4,3),Move (0,17),Move (0,18),Move (2,15),
                      Move (7,5),Move (0,19),Move (11,36),Move (9,38),
                      Move (8,24),Move (1,27),Move (1,34),Move (12,39),
                      Move (1,41),Move (0,20)]
        , testCase "test on grid3" $
            bfsSolve grid3 @?= (Nothing :: Maybe [Move])
        ]
    , testGroup "Tests for dfsSolve"
        [ testCase "test on grid1" $
            dfsSolve grid1 @?=
                Just [Move (0,16),Move (0,15),Move (1,6),Move (0,17),
                      Move (0,18),Move (0,19),Move (1,27),Move (1,34),
                      Move (0,17),Move (0,16),Move (0,15),Move (1,41),
                      Move (0,17),Move (0,18),Move (0,19),Move (0,20)]
        , testCase "size tests on grid1, grid2 and grid3" $
            map (fmap L.length . dfsSolve) [grid1,grid2,grid3] 
            @?= [Just 16,Just 978,Nothing]
        ]
    ]