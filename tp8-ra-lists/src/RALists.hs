{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module RALists where
import Data.IntMap (size)

-- natural integers
type Nat = Int

-- leaf labeled tree
data Tree a = Leaf a | Node Nat (Tree a) (Tree a) deriving (Show)

-- presence/absence of a complete binary tree
data Digit a = Zero | One (Tree a) deriving (Show)

-- Random access list
newtype RAList a = RAList {getDigits :: [Digit a]}

-- the number of leaves in the binary tree.
sizeT :: Tree a -> Nat
sizeT (Leaf _) = 1
sizeT (Node s _ _) = s

-- smart constructor
mkNodeT :: Tree a -> Tree a -> Tree a
mkNodeT t1 t2 = Node (sizeT t1 + sizeT t2) t1 t2

mkRA_ABCDE =
    RAList
        { getDigits =
            [ One (Leaf 'A')
            , Zero
            , One
                ( Node
                    4
                    (Node 2 (Leaf 'B') (Leaf 'C'))
                    (Node 2 (Leaf 'D') (Leaf 'E'))
                )
            ]
        }

mkRA_ABCDEF =
    RAList
        { getDigits =
            [ Zero
            , One (Node 2 (Leaf 'A') (Leaf 'B'))
            , One
                ( Node
                    4
                    (Node 2 (Leaf 'C') (Leaf 'D'))
                    (Node 2 (Leaf 'E') (Leaf 'F'))
                )
            ]
        }


-- Exercice 1 : Construction et déconstruction

nilRA :: RAList a
nilRA = undefined

nullRA :: RAList a -> Bool
nullRA = undefined

sizeRA :: RAList a -> Nat
sizeRA = undefined

fromT :: Tree a -> [a]
fromT = undefined

fromRA :: RAList a -> [a]
fromRA = undefined

instance Show a => Show (RAList a) where
    show = undefined

headRA' :: RAList a -> Maybe a
headRA' = undefined


-- Exercice 2 : Fonctions de base

consRA :: a -> RAList a -> RAList a
consRA = undefined

toRA :: [a] -> RAList a
toRA = undefined

unconsRA :: RAList a -> Maybe (a, RAList a)
unconsRA = undefined

headRA :: RAList a -> Maybe a
headRA = undefined

tailRA :: RAList a -> Maybe (RAList a)
tailRA = undefined

liftRA :: ([a] -> [b]) -> RAList a -> RAList b
liftRA f = toRA . f . fromRA

reverseRA :: RAList a -> RAList a
reverseRA = undefined


-- Exercice 3 : Accès indicé

fetchRA :: Nat -> RAList a -> a
fetchRA = undefined

safeFetchRA :: Nat -> RAList a -> Maybe a
safeFetchRA = undefined

updateRA :: Nat -> a -> RAList a -> RAList a
updateRA = undefined

instance Functor RAList where
    fmap = undefined

safeUpdateRA :: Nat -> a -> RAList a -> Maybe (RAList a)
safeUpdateRA = undefined


-- Exercice 4 : Pour aller beaucoup plus loin

safeUpdateRA' :: Nat -> a -> RAList a -> Maybe (RAList a)
safeUpdateRA' = undefined