module Tp6Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Tp6
import Data.List (sort, subsequences)

tests = testGroup "Tests for TP 6"
    [ ex1Tests
    , ex2Tests
    , ex3Tests
    , ex4Tests
    , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1 -- Échauffement"
    [ mTreeCountTests
    , mTreeLeavesTests
    , mTreeCountLeavesTests
    , mTreeSumTests
    , mTreeHeightTests
    , mTreeElemTests
    , mTreeMinTests
    , mTreeMaxTests
    , mTreeToListTests
    ]

mTreeCountTests = testGroup "Tests for mTreeCount"
    [ testCase "mTreeExample has 22 nodes" $
        mTreeCount mTreeExample @?= 22
    ]

mTreeLeavesTests = testGroup "Tests for mTreeLeaves"
    [ testCase "mTreeLeaves mTreeExample" $
        mTreeLeaves mTreeExample @?= [1,7,3,3,9,1,8,9,1,2,9,7,3]
    ]

mTreeCountLeavesTests = testGroup "Tests for mTreeCountLeaves"
    [ testCase "mTreeCountLeaves mTreeExample == 13" $
        mTreeCountLeaves mTreeExample @?= 13
    ]

mTreeSumTests = testGroup "Tests for mTreeSum"
    [ testCase "mTreeSum mTreeExample == 102" $
        mTreeSum mTreeExample @?= 102
    ]

mTreeHeightTests = testGroup "Tests for mTreeHeight"
    [ testCase "mTreeHeight mTreeExample == 4" $
        mTreeHeight mTreeExample @?= 4
    ]

mTreeElemTests = testGroup "Tests for mTreeElem"
    [ testCase "0 `mTreeElem` mTreeExample" $
        0 `mTreeElem` mTreeExample @?= False
    , testCase "5 `mTreeElem` mTreeExample" $
        5 `mTreeElem` mTreeExample @?= True
    , testCase "10 `mTreeElem` mTreeExample" $
        10 `mTreeElem` mTreeExample @?= False
    ]

mTreeMinTests = testGroup "Tests for mTreeMin"
    [ testCase "mTreeMin mTreeExample == 1" $
        mTreeMin mTreeExample @?= 1
    ]

mTreeMaxTests = testGroup "Tests for mTreeMax"
    [ testCase "mTreeMax mTreeExample == 9" $
        mTreeMax mTreeExample @?= 9
    ]

mTreeToListTests = testGroup "Tests for mTreeToList"
    [ testCase "mTreeToList mTreeExample (up to order)" $
        sort (mTreeToList mTreeExample)
        @?= sort [6,4,2,1,7,3,5,3,9,2,3,1,8,7,8,9,2,1,2,9,7,3]
    ]

ex2Tests = testGroup "Tests for exercise 2 -- Parcours"
    [ mTreeDepthFirstTraversalTests
    , mTreeBreadthFirstTraversalTests
    , mTreeLayerTests
    ]

mTreeDepthFirstTraversalTests = testGroup
    "Tests for mTreeDepthFirstTraversal"
    [ testCase "mTreeDepthFirstTraversal mTreeExample" $
        mTreeDepthFirstTraversal mTreeExample
        @?= [6,4,2,1,7,3,5,3,9,2,3,1,8,7,8,9,2,1,2,9,7,3]
    ]

mTreeBreadthFirstTraversalTests = testGroup
    "Tests for mTreeBreadthFirstTraversal"
    [ testCase "mTreeBreadthFirstTraversal mTreeExample" $
        mTreeBreadthFirstTraversal mTreeExample
        @?= [6,4,2,8,2,5,3,7,9,2,1,7,3,3,9,1,8,1,2,9,7,3]
    ]

mTreeLayerTests = testGroup
    "Tests for mTreeLayer"
    [ testCase "mTreeLayer 0 mTreeExample" $
        mTreeLayer 0 mTreeExample @?= ([]::[Integer])
    , testCase "mTreeLayer 1 mTreeExample" $
        mTreeLayer 1 mTreeExample @?= [6]
    , testCase "mTreeLayer 2 mTreeExample" $
        mTreeLayer 2 mTreeExample @?= [4,2,8]
    , testCase "mTreeLayer 3 mTreeExample" $
        mTreeLayer 3 mTreeExample @?= [2,5,3,7,9,2]
    , testCase "mTreeLayer 4 mTreeExample" $
        mTreeLayer 4 mTreeExample @?= [1,7,3,3,9,1,8,1,2,9,7,3]
    , testCase "mTreeLayer 5 mTreeExample" $
        mTreeLayer 5 mTreeExample @?= ([]::[Integer])
    ]

ex3Tests = testGroup "Tests for exercise 3 -- Fonctions d'ordre supérieur"
    [ mTreeMapTests
    , mTreeFilterTests
    , mTreeCollectPathsTests
    , mTreeSignatureTests
    ]

mTreeMapTests = testGroup
    "Tests for mTreeMap"
    [ testCase "mTreeMap (+ 10) mTreeExample" $
        mTreeMap (* 10) mTreeExample @?=
            MTree 60
            [ MTree 40
                [ MTree 20
                    [ MTree 10 []
                    , MTree 70 []
                    , MTree 30 []
                    ]
                , MTree 50
                    [ MTree 30 []
                    , MTree 90 []
                    ]
                ]
            , MTree 20
                [ MTree 30
                    [ MTree 10 []
                    ]
                ]
            , MTree 80
                [ MTree 70
                    [ MTree 80 []
                    ]
                , MTree 90 []
                , MTree 20
                    [ MTree 10 []
                    , MTree 20 []
                    , MTree 90 []
                    , MTree 70 []
                    , MTree 30 []
                    ]
                ]
            ]
    ]

mTreeFilterTests = testGroup
    "Tests for mTreeFilter"
    [ testCase "mTreeFilter even mTreeExample" $
        mTreeFilter even mTreeExample @?=
            [MTree 6
                [ MTree 4 [MTree 2 []]
                , MTree 2 []
                , MTree 8
                    [ MTree 8 []
                    , MTree 2 [MTree 2 []]
                    ]]]
    , testCase "mTreeFilter odd mTreeExample" $
        mTreeFilter odd mTreeExample @?=
            [MTree 1 [],MTree 7 [],MTree 3 []
            ,MTree 5 [ MTree 3 [], MTree 9 []]
            ,MTree 3 [ MTree 1 []]
            ,MTree 7 [],MTree 9 [],MTree 1 []
            ,MTree 9 [],MTree 7 [],MTree 3 []]
    , testCase "mTreeFilter (< 4) mTreeExample" $
        mTreeFilter (< 4) mTreeExample @?=
            [MTree 2
                [ MTree 1 []
                , MTree 3 []
                ],MTree 3 [],MTree 2
                [ MTree 3
                    [ MTree 1 []
                    ]
                ],MTree 2
                [ MTree 1 []
                , MTree 2 []
                , MTree 3 []
                ]]
    ]

mTreeCollectPathsTests = testGroup
    "Tests for mTreeCollectPaths"
    [ testCase "mTreeCollectPaths mTreeExample" $
        mTreeCollectPaths mTreeExample @?=
            [[6,4,2,1],[6,4,2,7],[6,4,2,3],
             [6,4,5,3],[6,4,5,9],[6,2,3,1],
             [6,8,7,8],[6,8,9],[6,8,2,1],
             [6,8,2,2],[6,8,2,9],[6,8,2,7],[6,8,2,3]]
    ]

mTreeSignatureTests = testGroup
    "Tests for mTreeSignature"
    [ testCase "mTreeSignature mTreeExample" $
        mTreeSignature mTreeExample @?=
           [13,19,15,18,24,12,29,23,17,18,25,23,19]
    ]

ex4Tests = testGroup "Tests for exercise 4 -- Générations"
    [ subsetsTests
    , permutedSubsetsTests
    , mTreesTests
    ]

subsetsTests = testGroup
    "Tests for subsets"
    [ testCase "subsets []" $
        subsets ([]::[Integer]) @?= ([[]]::[[Integer]])
    , testCase "subsets [1, 2, 3]" $
        sort (subsets [1, 2, 3]) @?= sort (subsequences [1, 2, 3])
    , testCase "subsets ['a', 'b', 'c', 'd']" $
        sort (subsets ['a', 'b', 'c', 'd']) @?=
            sort (subsequences ['a', 'b', 'c', 'd'])
    ]

permutedSubsetsTests = testGroup
    "Tests for permutedSubsets"
    [ testCase "permutedSubsets []" $
        permutedSubsets ([]::[Integer]) @?= ([[]]::[[Integer]])
    , testCase "permutedSubsets [1, 2]" $
        sort (permutedSubsets [1, 2]) @?=
            sort [[],[2],[1],[1,2],[2,1]]
    , testCase "permutedSubsets ['a', 'b', 'c']" $
        sort (permutedSubsets ['a', 'b', 'c']) @?=
            sort [[],['a'],['b'],['b','a'],['a','b'],['c'],
                  ['c','a'],['a','c'],['c','b'],['b','c'],
                  ['c','b','a'],['b','c','a'],['a','b','c'],
                  ['b','a','c'],['a','c','b'],['c','a','b']]
    ]

mTreesTests = testGroup
    "Tests for mTrees"
    [ testCase "mTrees []" $
        mTrees ([]::[Integer]) @?= ([]::[MTree Integer])
    , testCase "mTrees [1, 2]" $
        sort (mTrees [1, 2]) @?= sort [MTree 1 [MTree 2 []],
                                       MTree 2 [MTree 1 []]]
    , testCase "mTrees ['a', 'b', 'c']" $
        sort (mTrees ['a', 'b', 'c']) @?=
            sort [MTree 'a' [MTree 'b' [], MTree 'c' []],
                  MTree 'a' [MTree 'c' [], MTree 'b' []],
                  MTree 'a' [MTree 'b' [MTree 'c' []]],
                  MTree 'a' [MTree 'c' [MTree 'b' []]],
                  MTree 'b' [MTree 'a' [], MTree 'c' []],
                  MTree 'b' [MTree 'c' [], MTree 'a' []],
                  MTree 'b' [MTree 'a' [MTree 'c' []]],
                  MTree 'b' [MTree 'c' [MTree 'a' []]],
                  MTree 'c' [MTree 'a' [], MTree 'b' []],
                  MTree 'c' [MTree 'b' [], MTree 'a' []],
                  MTree 'c' [MTree 'a' [MTree 'b' []]],
                  MTree 'c' [MTree 'b' [MTree 'a' []]]]
 ]

ex5Tests = testGroup "Tests for exercise 5 -- Compléments"
    [ mTreeIsoTopologyTests
    , mTreeCutTests
    ]

mTreeIsoTopologyTests = testGroup
    "Tests for mTreeIsoTopology"
    [ testCase "simple test 1" $
        mTreeIsoTopology (MTree 1 []) (MTree 2 [])           @?= True
    , testCase "simple test 2" $
        mTreeIsoTopology (MTree 1 [MTree 3 []]) (MTree 2 []) @?= False
    , testCase "simple test 3" $
        mTreeIsoTopology (MTree 1 [])
                         (MTree 2 [MTree 3 []])              @?= False
    , testCase "simple test 4" $
        mTreeIsoTopology (MTree 1 [MTree 3 [], MTree 4 []])
                         (MTree 2 [MTree 3 []])              @?= False
    , testCase "simple test 5" $
        mTreeIsoTopology (MTree 1 [MTree 3 [], MTree 4 []])
                         (MTree 2 [MTree 3 [], MTree 5 []])  @?= True
    , testCase "mTreeIsoTopology mTreeExample mTreeExample" $
        mTreeIsoTopology mTreeExample mTreeExample @?= True
    , testCase "mTreeIsoTopology mTreeExample (mTreeMap (+ 1) mTreeExample)" $
        mTreeIsoTopology mTreeExample (mTreeMap (+ 1) mTreeExample) @?= True
    , testCase "complex example 1" $
        map (mTreeIsoTopology mTreeExample)
            (mTreeFilter (/= 6) mTreeExample) @?= [False, False, False]
    , testCase "complex example 2" $
        map (mTreeIsoTopology mTreeExample)
            (mTreeFilter (/= 10) mTreeExample) @?= [True]
    ]

mTreeCutTests = testGroup
    "Tests for mTreeCut"
    [ testCase "mTreeCut 1 mTreeExample" $
        mTreeCut 1 mTreeExample @?= MTree 6 []
    , testCase "mTreeCut 2 mTreeExample" $
        mTreeCut 2 mTreeExample @?= 
            MTree 6 [MTree 4 [],MTree 2 [],MTree 8 []]
    , testCase "mTreeCut 3 mTreeExample" $
        mTreeCut 3 mTreeExample @?= 
            MTree 6 [MTree 4 [MTree 2 [],MTree 5 []],
                     MTree 2 [MTree 3 []],
                     MTree 8 [MTree 7 [],MTree 9 [],MTree 2 []]]
    ]

