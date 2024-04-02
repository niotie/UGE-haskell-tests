module Tp5 where
import qualified Data.List as L
import Data.Foldable (Foldable(foldl'))

data BSTree a = Node (BSTree a) a (BSTree a) | Empty
                         deriving (Eq)

instance Show a => Show (BSTree a) where
    show = (++ "\n") . ("\n" ++) . showBSTree2

showBSTree _ Empty          = []
showBSTree n (Node lt x rt) = L.replicate n '*'   ++
                                show x ++ "\n"      ++
                                showBSTree (n+4) lt ++
                                showBSTree (n+4) rt


showBSTree2 :: Show a => BSTree a -> String
showBSTree2 Empty = "<empty tree>"
showBSTree2 t = L.intercalate "\n" $ aux [] t
    where aux _ Empty = []
          aux path (Node lt x rt) =
              aux (0:path) rt ++
              (prefix path ++ show x) :
              aux (1:path) lt
          prefix [] = ""
          prefix xs@(x:xs') = (concat . reverse $ zipWith bracket xs xs') ++ "+-- "
          bracket x y = if x + y == 1 then "|   " else "    "

-- Exercise 1

mkExampleBSTree = undefined

countNodesBSTree = undefined

countLeavesBSTree = undefined

heightBSTree = undefined

leavesBSTree = undefined

elemBSTree = undefined

-- Exercise 2

inOrderVisitBSTree = undefined

preOrderVisitBSTree = undefined

postOrderVisitBSTree = undefined


-- Exercise 3

insertBSTree = undefined

fromListBSTree = undefined

toListBSTree = undefined

mergeBSTree = undefined


-- Exercise 4

leftmostBSTree = undefined

minBSTree = undefined

rightmostBSTree = undefined

maxBSTree = undefined

deleteRootBSTree = undefined

extractMax = undefined

deleteBSTree = undefined


-- Exercise 5

mapBSTree1 = undefined

filterBSTree = undefined

foldBSTree = undefined

countNodesBSTree2 = undefined

inOrderVisitBSTree2 = undefined


-- Exercise 6

genBSTrees = undefined


-- Exercise 7

isBSTree = undefined

maxBST = undefined

minBST = undefined

isBSTree' = undefined

pathsBSTree = undefined

fromPathsBSTree = undefined

pruneBSTree = undefined
