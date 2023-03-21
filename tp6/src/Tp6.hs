module Tp6 where

data MTree a = MTree {rootLabel :: a, subForest :: MForest a}
    deriving (Eq, Ord)

type MForest a = [MTree a]

mTreeIndent = 4
mTreeBranchChar = '.'
mTreeNodeChar = '+'

instance (Show a) => Show (MTree a) where

    show = go 0
        where
        go nTabs MTree {rootLabel = rl, subForest = mts} =
            replicate nTabs mTreeBranchChar ++
            (if nTabs > 0 then " " else "") ++
            mTreeNodeChar:" root label=" ++
            show rl ++
            "\n" ++
            foldr f "" mts
            where
            f mt acc = go (nTabs + mTreeIndent) mt ++ acc

mTreeMk rl mts = MTree {rootLabel = rl, subForest = mts}
mTreeMkLeaf rl = mTreeMk rl []

mTreeExample :: MTree Integer
mTreeExample = root
    where
    root = mt_01_01_6
    -- writing convention : mt_level_left-to-right-index_value
    -- level 01
    mt_01_01_6 = mTreeMk 6 [mt_02_01_4, mt_02_02_2, mt_02_03_8]
    -- level 02
    mt_02_01_4 = mTreeMk 4 [mt_03_01_2, mt_03_02_5]
    mt_02_02_2 = mTreeMk 2 [mt_03_03_3]
    mt_02_03_8 = mTreeMk 8 [mt_03_04_7, mt_03_05_9, mt_03_06_2]
    -- level 03
    mt_03_01_2 = mTreeMk 2 [mt_04_01_1, mt_04_02_7, mt_04_03_3]
    mt_03_02_5 = mTreeMk 5 [mt_04_04_3, mt_04_05_9]
    mt_03_03_3 = mTreeMk 3 [mt_04_06_1]
    mt_03_04_7 = mTreeMk 7 [mt_04_07_8]
    mt_03_05_9 = mTreeMkLeaf 9
    mt_03_06_2 = mTreeMk 2 [mt_04_08_1, mt_04_09_2, mt_04_10_9,
                            mt_04_11_7, mt_04_12_3]
    -- level 04
    mt_04_01_1 = mTreeMkLeaf 1
    mt_04_02_7 = mTreeMkLeaf 7
    mt_04_03_3 = mTreeMkLeaf 3
    mt_04_04_3 = mTreeMkLeaf 3
    mt_04_05_9 = mTreeMkLeaf 9
    mt_04_06_1 = mTreeMkLeaf 1
    mt_04_07_8 = mTreeMkLeaf 8
    mt_04_08_1 = mTreeMkLeaf 1
    mt_04_09_2 = mTreeMkLeaf 2
    mt_04_10_9 = mTreeMkLeaf 9
    mt_04_11_7 = mTreeMkLeaf 7
    mt_04_12_3 = mTreeMkLeaf 3


-- Exercice 1

mTreeCount = undefined

mTreeIsLeaf = undefined

mTreeLeaves = undefined

mTreeCountLeaves = undefined

mTreeSum = undefined

mTreeHeight = undefined

mTreeElem = undefined

mTreeMin = undefined

mTreeMax = undefined

mTreeToList = undefined


-- Exercice 2

mTreeDepthFirstTraversal = undefined

mTreeBreadthFirstTraversal = undefined

mForestBreadthFirstTraversal = undefined

mTreeLayer = undefined


-- Exercice 3

mTreeMap = undefined

mTreeFilter = undefined

mTreeFold1 f z (MTree rl []) = [f rl z]
mTreeFold1 f z (MTree rl mts) =
    [f rl y | mt' <- mts, y <- mTreeFold1 f z mt']

mTreeCollectPaths = undefined

mTreeSignature = undefined

mTreeFold2 f (MTree rl mts) = f rl xs
    where xs = map (mTreeFold2 f) mts

mTreeMin' :: Ord a => MTree a -> a
mTreeMin' = undefined

mTreeMax' :: Ord a => MTree a -> a
mTreeMax' = undefined

mTreeSum' :: Num a => MTree a -> a
mTreeSum' = undefined

mTreeToList' = undefined


-- Exercice 4 - Générations

subsets = undefined

permutedSubsets = undefined