module Tp9 where

import qualified Data.Char
import qualified Data.Foldable as F
import qualified Data.List     as L
import           Data.Monoid

data FType = Img | Txt | Mov deriving (Show)

type Name = String

data FSItem = File { fName :: Name
                   , fType :: FType
                   , fSize :: Int
                     }
            | Dir  { dName  :: Name
                   , fSItems :: [FSItem]
                     }

data FSCrumb = FSCrumb { fSFocusName       :: Name     -- directory name
                       , fSFocusItemsLeft  :: [FSItem] -- file systems items left of focus element
                       , fSFocusItemsRight :: [FSItem] -- file systems items right of focus element
                       } deriving (Show)

newtype FSFocus = FSFocus (FSItem, [FSCrumb]) deriving (Show)

instance Show FSItem where
  show (File { fName = n, fType = t, fSize = s }) = n ++ "." ++ show t ++ " (" ++ show s ++ ")"
  show (Dir  { dName = n }) = n ++ "/"

fSMkDir :: String -> [FSItem] -> FSItem
fSMkDir n fis = Dir { dName = n, fSItems = fis }

fSMkFile :: FType -> String -> Int -> FSItem
fSMkFile t n s = File { fName = n, fType = t, fSize = s }

fSMkImgFile :: String -> Int -> FSItem
fSMkImgFile = fSMkFile Img

fSMkTxtFile :: String -> Int -> FSItem
fSMkTxtFile = fSMkFile Txt

fSMkMovFile :: String -> Int -> FSItem
fSMkMovFile = fSMkFile Mov

myDisk :: FSItem
myDisk = root
  where
    -- directories
    root = fSMkDir "root" [fa,fb,d1,d2,d3,fc]
    d1 = fSMkDir "d1"  [fd,fe,d4]
    d2 = fSMkDir "d2" [fg,fh,fi,fj]
    d3 = fSMkDir "d3" [d5]
    d4 = fSMkDir "d4" [ff]
    d5 = fSMkDir "d5" [d6,d7,d8]
    d6 = fSMkDir "d6" [fk,fl]
    d7 = fSMkDir "d7" [d9]
    d8 = fSMkDir "d8" []
    d9 = fSMkDir "d9" [fm,fn,fo]

    -- fSFiles
    fa = fSMkTxtFile "fa" (2^6)
    fb = fSMkTxtFile "fb" (2^7)
    fc = fSMkImgFile "fc" (2^8)
    fd = fSMkTxtFile "fd" (2^3)
    fe = fSMkImgFile "fe" (2^8)
    ff = fSMkMovFile "ff" (2^12)
    fg = fSMkTxtFile "fg" (2^10)
    fh = fSMkImgFile "fh" (2^9)
    fi = fSMkMovFile "fi" (2^11)
    fj = fSMkMovFile "fj" (2^13)
    fk = fSMkImgFile "fk" (2^7)
    fl = fSMkMovFile "fl" (2^12)
    fm = fSMkImgFile "fm" (2^5) 
    fn = fSMkTxtFile "fn" (2^4)
    fo = fSMkTxtFile "fo" (2^3)


-- Exercice 1

fSIsImgFile :: FSItem -> Bool
fSIsImgFile = undefined

fSIsTxtFile :: FSItem -> Bool
fSIsTxtFile = undefined

fSIsMovFile :: FSItem -> Bool
fSIsMovFile = undefined

fSFiles :: FSItem -> [FSItem]
fSFiles = undefined

fSImgFiles :: FSItem -> [FSItem]
fSImgFiles = undefined

fSTxtFiles :: FSItem -> [FSItem]
fSTxtFiles = undefined

fSMovFiles :: FSItem -> [FSItem]
fSMovFiles = undefined

fSTotalSize :: FSItem -> Int
fSTotalSize = undefined

fSDepth :: (Num a, Ord a) => FSItem -> a
fSDepth = undefined

maximumBy :: Ord b => (a -> b) -> [a] -> Maybe a
maximumBy = undefined

fSMaximumFileSize :: FSItem -> Maybe FSItem
fSMaximumFileSize = undefined


-- Exercice 2

fSElem :: Name -> FSItem -> Bool
fSElem = undefined

fSNotElem :: Name -> FSItem -> Bool
fSNotElem = undefined

fSElemRec :: Name -> FSItem -> Bool
fSElemRec = undefined

fSNotElemRec :: Name -> FSItem -> Bool
fSNotElemRec = undefined

fSPaths :: FSItem -> [String]
fSPaths = undefined

mTreeFold1 = undefined

fSLs :: FSItem -> Maybe [FSItem]
fSLs = undefined


-- Exercice 3

fSAdd :: FSItem -> FSItem -> Maybe FSItem
fSAdd = undefined

fSRm :: Name -> FSItem -> Maybe FSItem
fSRm = undefined

fSMkBackup :: Name -> FSItem -> FSItem
fSMkBackup = undefined

fSMap :: (FSItem -> FSItem) -> FSItem -> FSItem
fSMap = undefined

capName :: Name -> Name
capName [] = []
capName (x:xs) = Data.Char.toUpper x : L.map Data.Char.toLower xs

fSMapCapitalizeFiles :: FSItem -> FSItem
fSMapCapitalizeFiles f@File { fName = n } = f { fName = capName n }
fSMapCapitalizeFiles d = d

fSMapCapitalizeDirs :: FSItem -> FSItem
fSMapCapitalizeDirs d@Dir { dName = n } = d { dName = capName n }
fSMapCapitalizeDirs f = f

fSMapCapitalize :: FSItem -> FSItem
fSMapCapitalize f@File { fName = n } = f { fName = capName n }
fSMapCapitalize d@Dir { dName = n } = d { dName = capName n }

fSFindByName :: Name -> [FSItem] -> Maybe FSItem
fSFindByName = undefined


-- Exercice 4

-- make a new focus on a fs element
fSFocusMk :: FSItem -> FSFocus
fSFocusMk fsi = FSFocus (fsi, [])

-- get the fs element under focus
fSFocusGet :: FSFocus -> FSItem
fSFocusGet (FSFocus (f, _)) = f

(-:) :: a -> (a -> b) -> b
x -: f = f x

fSFocusTo :: Name -> FSFocus -> FSFocus
fSFocusTo = undefined

fSFocusUp :: FSFocus -> FSFocus
fSFocusUp = undefined

fSFocusRoot :: FSFocus -> FSFocus
fSFocusRoot = undefined

fSFocusRename :: Name -> FSFocus -> FSFocus
fSFocusRename = undefined

fSFocusNew :: FSItem -> FSFocus -> Maybe FSFocus
fSFocusNew = undefined
