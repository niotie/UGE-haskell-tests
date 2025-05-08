module Tpfinal where

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Exercice 1 : Listes

countPeaks :: (Ord a) => [a] -> Int
countPeaks = undefined

runs :: (Ord a) => [a] -> [[a]]
runs = undefined

flatten :: [[a]] -> [a]
flatten = undefined

semiPalindrome :: (Eq a) => [a] -> Bool
semiPalindrome = undefined

shuffles :: [a] -> [a] -> [[a]]
shuffles = undefined


-- Exercice 2 : Des missionnaires et des cannibales

-- Maximum umber of persons on the boat
type BoatCapacity = Int

-- number of missionaries and cannibals, resp.
type NMissionaries = Int
type NCannibals = Int

-- Booat is either on the left bank (L) or on the right bank (R)
data Boat = L | R deriving (Show, Eq, Ord)

-- A group of missionaries and cannibals
data Group = Group { nM :: NMissionaries, nC :: NCannibals } deriving (Show, Eq, Ord)

-- The whole configuration:
-- lGroup is the group on the left bank
-- rGroup is the group on the right bank
-- boat indicates where is the boat
data Configuration = Configuration { lGroup :: Group
                                   , rGroup :: Group
                                   , boat :: Boat
                                   } deriving (Show, Eq, Ord)

-- A list of configurations.
-- The initial configuration is the first configuration.
-- The i-th configuration is achievable by a legal move from the (i-1)-th configuration.
type Seq = [Configuration]

-- convenient Group constructor
mkGroup :: NMissionaries -> NCannibals -> Group
mkGroup nM nC = Group { nM = nM, nC = nC }

-- make the null group
emptyGroup :: Group
emptyGroup = mkGroup 0 0

-- test if a group is empty
nullGroup :: Group -> Bool
nullGroup g = nM g == 0 && nC g == 0

-- convenient Configuration constructor
mkConfiguration :: Group -> Group -> Boat -> Configuration
mkConfiguration lG rG b = Configuration { lGroup = lG, rGroup = rG, boat = b }

-- make an initial configuration with given numbers of missionaries and cannibals
initialConfiguration :: Int -> Int -> Configuration
initialConfiguration nM nC = Configuration { lGroup = mkGroup nM nC
                                           , rGroup = emptyGroup
                                           , boat = L }

finalConfiguration :: Configuration -> Bool
finalConfiguration = undefined

reverseConfiguration :: Configuration -> Configuration
reverseConfiguration = undefined

legalMoves :: BoatCapacity -> Configuration -> [Configuration]
legalMoves = undefined

extend :: BoatCapacity -> Seq -> [Seq]
extend = undefined

solve :: BoatCapacity -> Configuration -> [Seq]
solve = undefined

solveBest :: BoatCapacity -> Configuration -> Maybe Seq
solveBest = undefined

opt :: Configuration -> Int
opt = undefined


-- Exercice 3 : Maps

type MyMap a = M.Map a (S.Set a)

myMap1 :: MyMap Int
myMap1 = M.fromList [(1, S.singleton 2), (2, S.singleton 3), (3, S.fromList [2,3,4])]

myMap2 :: MyMap Int
myMap2 = M.fromList [(2, S.singleton 2), (3, S.fromList [1,2,3]), (4, S.singleton 1)]

myMap3 :: MyMap Int
myMap3 = M.fromList [(1, S.singleton 2), (3, S.singleton 3), (4, S.fromList [1,3,5])]

fusionMap :: (Ord a) => MyMap a -> MyMap a -> MyMap a
fusionMap = undefined

lookupMap :: Ord a => a -> MyMap a -> Maybe [a]
lookupMap = undefined

reverseMap :: (Ord a) => MyMap a -> MyMap a
reverseMap = undefined


-- Exercice 4 : Run-Length Encoding

-- run
type RLERun = (Int, Char)

-- (Char) run-length encoded list
data RLEList = ConsRLEList RLERun RLEList | NilRLEList deriving (Eq, Ord)

instance Show RLEList where
  show = show . toListRLEList
    where
      toListRLEList NilRLEList = []
      toListRLEList (ConsRLEList rleE rleL) = toListRLERun rleE ++ toListRLEList rleL

mkRLERun :: Int -> Char -> RLERun
mkRLERun = (,)

toListRLERun :: RLERun -> String
toListRLERun = uncurry L.replicate

showRLERun :: RLERun -> String
showRLERun = toListRLERun

showRLEList :: RLEList -> String
showRLEList NilRLEList = ""
showRLEList (ConsRLEList r rlel) = show r ++ showRLEList rlel

emptyRLEList :: RLEList
emptyRLEList = NilRLEList

incRLERun :: RLERun -> RLERun
incRLERun = undefined

decRLERun :: RLERun -> RLERun
decRLERun = undefined

consRLEList :: Char -> RLEList -> RLEList
consRLEList = undefined

fromListRLEList :: [Char] -> RLEList
fromListRLEList = undefined

headRLEList :: RLEList -> Maybe Char
headRLEList = undefined

tailRLEList :: RLEList -> Maybe (RLEList)
tailRLEList = undefined

initRLEList :: RLEList -> Maybe (RLEList)
initRLEList = undefined

(@++@) :: RLEList -> RLEList -> RLEList
(@++@) = undefined

mapRLEList :: (Char -> Char) -> RLEList -> RLEList
mapRLEList = undefined

