module Tpfinal where

import Data.List as L

----------------------------------------------------------

-- Exercice 1

-- combinations1 :: (Eq b, Num b) => b -> [a] -> [[a]]

-- combinations2 :: Int -> [a] -> [[a]]

-- combinations3 :: (Eq b, Num b) => b -> [a] -> [[a]]

----------------------------------------------------------

-- Exercice 2

data BTree a = Empty | Branch (BTree a) a (BTree a)

indent :: Int
indent = 4

instance Show a => Show (BTree a) where
  show = go 0
    where
      go _ (Empty) = []
      go n (Branch lt x rt) = L.replicate n '*'   ++
                              show x ++ "\n"      ++
                              go (n+indent) lt ++
                              go (n+indent) rt

mkLeaf :: a -> BTree a
mkLeaf x = Branch Empty x Empty

-- nodes :: BTree a -> [a]

-- uniq :: Eq a => [a] -> [a]


-- isDuplicateFree :: Eq a => BTree a -> Bool

-- labelElem :: Eq a => a -> BTree a -> Bool

-- isSubtree :: Eq a => BTree a -> BTree a -> Bool

-- pathsFromRoot :: BTree a -> [[a]]

-- isBalanced :: BTree a -> Bool

----------------------------------------------------------

-- Exercice 3

data Queue a = Queue { inQ :: [a], outQ :: [a] } deriving (Eq)

instance Show a => Show (Queue a) where
  show Queue { inQ = xs, outQ = ys } = "Q[" ++ L.intercalate ">" (map show (xs ++ L.reverse ys)) ++ "]"

-- emptyQ :: Queue a

-- isEmptyQ :: Queue a -> Bool

-- pushQ :: a -> Queue a -> Queue a

-- popQ :: Queue a -> (Maybe a, Queue a)

-- fromListQ :: [a] -> Queue a

-- toListQ :: Queue a -> [a]

-- popWhileQ :: (a -> Bool) -> Queue a -> ([a],Queue a)

----------------------------------------------------------

-- Exercice 4

type AList k v = [(k, v)]

-- aListDelete :: Eq k => k -> AList k v -> AList k v

-- aListLookup :: Eq k => k -> AList k v -> Maybe v

-- aListInvLookup :: Eq v => v -> AList k v -> [k]

-- aListUpdate :: Eq k => k -> v -> AList k v -> AList k v

-- aListMerge :: Eq k => AList k v -> AList k v -> AList k v

-- aListFunUpdate :: Eq l => (k -> l) -> AList k v -> AList l v

----------------------------------------------------------
