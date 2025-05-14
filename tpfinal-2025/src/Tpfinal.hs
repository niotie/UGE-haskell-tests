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

interleavings :: [a] -> [a] -> [[a]]
interleavings = undefined

squareInterleaving :: (Eq a) => [a] -> Bool
squareInterleaving = undefined

-- Exercice 2 : Maps

type MyMap a = M.Map a (S.Set a)

myMap1 :: MyMap Int
myMap1 =
  M.fromList
    [ (1, S.singleton 2),
      (2, S.singleton 3),
      (3, S.fromList [2, 3, 4])
    ]

myMap2 :: MyMap Int
myMap2 =
  M.fromList
    [ (2, S.singleton 2),
      (3, S.fromList [1, 2, 3]),
      (4, S.singleton 1)
    ]

myMap3 :: MyMap Int
myMap3 =
  M.fromList
    [ (1, S.singleton 2),
      (3, S.singleton 3),
      (4, S.fromList [1, 3, 5])
    ]

singlesMyMap :: (Ord a) => [a] -> MyMap a
singlesMyMap = undefined

addMyMap :: (Ord a) => a -> MyMap a -> MyMap a
addMyMap = undefined

onlySingletonMyMap :: MyMap a -> MyMap a
onlySingletonMyMap = undefined

fusionMyMap :: (Ord a) => MyMap a -> MyMap a -> MyMap a
fusionMyMap = undefined

lookupMyMap :: (Ord a) => a -> MyMap a -> Maybe [a]
lookupMyMap = undefined

reverseMyMap :: (Ord a) => MyMap a -> MyMap a
reverseMyMap = undefined

representativeMyMap :: (Ord a) => MyMap a -> MyMap a
representativeMyMap = undefined

-- Exercice 3 : Run-Length Encoding

-- RLE Run
type RLER = (Int, Char)

-- RLE String (i.e. Char run-length encoded list)
data RLES = Cons RLER RLES | Nil deriving (Eq, Ord)

instance Show RLES where
  show = show . toListRLES

toListRLES :: RLES -> String
toListRLES Nil = []
toListRLES (Cons rleE rles) = toListRLER rleE ++ toListRLES rles

mkRLER :: Int -> Char -> RLER
mkRLER = (,)

toListRLER :: RLER -> String
toListRLER = uncurry L.replicate

showRLER :: RLER -> String
showRLER = toListRLER

showRLES :: RLES -> String
showRLES Nil = ""
showRLES (Cons r rles) = show r ++ showRLES rles

emptyRLES :: RLES
emptyRLES = Nil

incRLER :: RLER -> RLER
incRLER = undefined

decRLER :: RLER -> RLER
decRLER = undefined

consRLES :: Char -> RLES -> RLES
consRLES = undefined

toRLES :: String -> RLES
toRLES = undefined

lengthRLES :: RLES -> Int
lengthRLES = undefined

headRLES :: RLES -> Maybe Char
headRLES = undefined

tailRLES :: RLES -> Maybe RLES
tailRLES = undefined

initRLES :: RLES -> Maybe RLES
initRLES = undefined

(@++@) :: RLES -> RLES -> RLES
(@++@) = undefined

mapRLES :: (Char -> Char) -> RLES -> RLES
mapRLES = undefined

palindromeRLES :: RLES -> Bool
palindromeRLES = undefined

takeRLES :: Int -> RLES -> RLES
takeRLES = undefined

dropRLES :: Int -> RLES -> RLES
dropRLES = undefined

splitAtRLES :: Int -> RLES -> (RLES, RLES)
splitAtRLES = undefined

foldrRLES :: (RLER -> a -> a) -> a -> RLES -> a
foldrRLES f acc Nil = acc
foldrRLES f acc (Cons rler rlel) = f rler (foldrRLES f acc rlel)

countRLES :: Char -> RLES -> Int
countRLES = undefined

mapRLES' :: (Char -> Char) -> RLES -> RLES
mapRLES' = undefined

-- Exercice supprimé : Des missionnaires et des cannibales

-- Maximum umber of persons on the boat
type BoatCapacity = Int

-- number of missionaries and cannibals, resp.
type NMissionaries = Int

type NCannibals = Int

-- Booat is either on the left bank (L) or on the right bank (R)
data Boat = L | R deriving (Show, Eq, Ord)

-- A group of missionaries and cannibals
data Group = Group {nM :: NMissionaries, nC :: NCannibals} deriving (Show, Eq, Ord)

-- The whole configuration:
-- lGroup is the group on the left bank
-- rGroup is the group on the right bank
-- boat indicates where is the boat
data Configuration = Configuration
  { lGroup :: Group,
    rGroup :: Group,
    boat :: Boat
  }
  deriving (Show, Eq, Ord)

-- A list of configurations.
-- The initial configuration is the first configuration.
-- The i-th configuration is achievable by a legal move from the (i-1)-th configuration.
type Seq = [Configuration]

-- convenient Group constructor
mkGroup :: NMissionaries -> NCannibals -> Group
mkGroup nM nC = Group {nM = nM, nC = nC}

-- make the null group
emptyGroup :: Group
emptyGroup = mkGroup 0 0

-- test if a group is empty
nullGroup :: Group -> Bool
nullGroup g = nM g == 0 && nC g == 0

-- convenient Configuration constructor
mkConfiguration :: Group -> Group -> Boat -> Configuration
mkConfiguration lG rG b = Configuration {lGroup = lG, rGroup = rG, boat = b}

-- make an initial configuration with given numbers of missionaries and cannibals
initialConfiguration :: Int -> Int -> Configuration
initialConfiguration nM nC =
  Configuration
    { lGroup = mkGroup nM nC,
      rGroup = emptyGroup,
      boat = L
    }

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
