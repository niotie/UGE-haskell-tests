{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Tp9 where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Maybe    as Maybe
import qualified Data.Set      as S

import qualified Control.Monad.State as State
import Data.Foldable (Foldable(toList))
import Foreign (withArray)

type ISBN     = String
type Title    = String
type Author   = String
type Serial   = Int

type Action = ISBN -> Serial -> Library -> Library

data EAction = DeleteBook ISBN Serial
             | AddBook    ISBN Title Author
             | BorrowBook ISBN Serial
             | ReturnBook ISBN Serial

data Book = Book { getISBN :: ISBN,
                   getTitle :: Title,
                   getAuthor :: Author }
            deriving (Eq, Ord)

data BookRecord = BookRecord { getBook          :: Book
                             , getShelvedBooks  :: S.Set Serial
                             , getBorrowedBooks :: S.Set Serial }

newtype Library = Library { getBookRecords :: M.Map ISBN BookRecord }

instance Show Book where
  show b = "(" ++ L.intercalate ", " [getAuthor b, getTitle b, getISBN b] ++ ")"

instance Show Library where
  show lib = "+- Shelved books ----------------------------------------------------+\n" ++
             reshape shelvedBooks ++ "\n" ++
             "+- Borrowed books ---------------------------------------------------+\n" ++
             reshape  borrowedBooks ++
             "\n+--------------------------------------------------------------------+"
    where
      reshape = L.intercalate "\n" . L.map ("| " ++) . L.filter (not . L.null)
      (shelvedBooks, borrowedBooks) = F.foldr f ([], []) . M.elems . getBookRecords $ lib
        where
          f br (accShelvedBooks, accBorrowedBooks) = (accShelvedBooks', accBorrowedBooks')
            where
              collect s
                | S.null s = ""
                | otherwise = show (getBook br) ++ " / " ++ show (S.toList s)
              accShelvedBooks'  = collect (getShelvedBooks  br) : accShelvedBooks
              accBorrowedBooks' = collect (getBorrowedBooks br) : accBorrowedBooks

mkBook :: ISBN -> Title -> Author -> Book
mkBook isbn t a = Book { getISBN = isbn, getTitle = t, getAuthor = a }

mkBookRecord :: Book -> [Serial] -> [Serial] -> BookRecord
mkBookRecord b sShelvedBooks sBorrowedBooks =
  BookRecord { getBook = b
             , getShelvedBooks  = S.fromList sShelvedBooks
             , getBorrowedBooks = S.fromList sBorrowedBooks }

myLib :: Library
myLib = Library { getBookRecords = M.fromList brs }
  where
    brs =
      [
        (getISBN malevil,               mkBookRecord malevil                [1,2]    [])
      , (getISBN weekEndZuydcoote,      mkBookRecord weekEndZuydcoote         [1]    [])
      , (getISBN madrapour,             mkBookRecord madrapour                [1]    [])
      , (getISBN laNuitDesTemps,        mkBookRecord laNuitDesTemps           [2] [1,3])
      , (getISBN ravage,                mkBookRecord ravage                   [1]    [])
      , (getISBN voyageurImprudent,     mkBookRecord voyageurImprudent        [1]    [])
      , (getISBN machineExplorerTemps,  mkBookRecord machineExplorerTemps     [2]   [1])
      , (getISBN ileDocteurMoreau,      mkBookRecord ileDocteurMoreau         [1]    [])
      , (getISBN laCouleurTombeeDuCiel, mkBookRecord laCouleurTombeeDuCiel     []   [1])
      , (getISBN jeSuisDAilleurs,       mkBookRecord jeSuisDAilleurs          [1]    [])
      , (getISBN laCarteTerritoire1,    mkBookRecord laCarteTerritoire1       [1]    [])
      , (getISBN laCarteTerritoire2,    mkBookRecord laCarteTerritoire2        []   [3])
      ]

myBooks :: [Book]
myBooks = L.concat
  [ L.replicate 2 (mkBook "978-2070374441" "Malevil" "R. Merle")
  ,               [mkBook "978-2070367757" "Week-end a Zuydcoote" "R. Merle"]
  ,               [mkBook "978-2020336512" "Madrapour" "R. Merle"]
  , L.replicate 3 (mkBook "978-2266230919" "La nuit des temps" "R. Barjavel")
  ,               [mkBook "978-2070362387" "Ravage" "R. Barjavel"]
  ,               [mkBook "978-2070364855" "Le voyageur imprudent" "R. Barjavel"]
  , L.replicate 2 (mkBook "978-2070782109" "La machine a explorer le temps" "H. G. Wells")
  ,               [mkBook "978-2070401789" "L'ile du docteur Moreau" "H. G. Wells"]
  ,               [mkBook "978-2070415809" "La couleur tombee du ciel" "H. P. Lovecraft"]
  ,               [mkBook "978-2070421206" "Je suis d'ailleurs" "H. P. Lovecraft"]
  ,               [mkBook "978-2070367757" "Week-end a Zuydcoote" "R. Merle"]
  ,               [mkBook "978-2919695010" "La carte et le territoire" "M. Levy"]
  ,               [mkBook "978-2290032039" "La carte et le territoire" "M. Houellebecq"]
  ]

malevil :: Book
malevil = mkBook "978-2070374441" "Malevil" "R. Merle"

weekEndZuydcoote :: Book
weekEndZuydcoote = mkBook "978-2070367757" "Week-end a Zuydcoote"  "R. Merle"

madrapour :: Book
madrapour = mkBook "978-2020336512" "Madrapour" "R. Merle"

laNuitDesTemps :: Book
laNuitDesTemps = mkBook "978-2266230919" "La nuit des temps" "R. Barjavel"

ravage :: Book
ravage = mkBook "978-2070362387" "Ravage" "R. Barjavel"

voyageurImprudent :: Book
voyageurImprudent = mkBook "978-2070364855" "Le voyageur imprudent" "R. Barjavel"

machineExplorerTemps :: Book
machineExplorerTemps = mkBook "978-2070782109" "La machine a explorer le temps" "H. G. Wells"

ileDocteurMoreau :: Book
ileDocteurMoreau = mkBook "978-2070401789" "L'ile du docteur Moreau" "H. G. Wells"

laCouleurTombeeDuCiel :: Book
laCouleurTombeeDuCiel = mkBook "978-2070415809" "La couleur tombee du ciel" "H. P. Lovecraft"

jeSuisDAilleurs :: Book
jeSuisDAilleurs = mkBook "978-2070421206" "Je suis d'ailleurs" "H. P. Lovecraft"

laCarteTerritoire1 :: Book
laCarteTerritoire1 = mkBook "978-2919695010" "La carte et le territoire" "M. Levy"

laCarteTerritoire2 :: Book
laCarteTerritoire2 = mkBook "978-2290032039" "La carte et le territoire" "M. Houellebecq"

myScenario :: [(Action, ISBN, Serial)]
myScenario =
  [ (borrowBook, "978-2020336512", 1) -- borrow R. Merle, Madrapour, copy 1
  , (borrowBook, "978-2266230919", 2) -- borrow R. Barjavel, La nuit des temps, copy 2
  , (returnBook, "978-2020336512", 1) -- return R. Merle, Madrapour, copy 1
  , (borrowBook, "978-2070367757", 1) -- borrow R. Merle, Week-end a Zuydcoote, copy 1
  , (returnBook, "978-2266230919", 2) -- return R. Barjavel, La nuit des temps, copy 2,
  , (returnBook, "978-2070367757", 1) -- return R. Merle, Week-end a Zuydcoote, copy 1
  , (borrowBook, "978-2070401789", 1) -- borrow H. G. Wells, L'ile du docteur Moreau, copy 1
  , (borrowBook, "978-2070421206", 1) -- borrow H. P. Lovecraft, Je suis d'ailleurs, copy 1
  ]

myEScenario :: [EAction]
myEScenario =
  [ BorrowBook "978-2020336512" 1     -- borrow R. Merle, Madrapour, copy 1
  , BorrowBook "978-2266230919" 2 -- borrow R. Barjavel, La nuit des temps, copy 2
  , ReturnBook "978-2020336512" 1 -- return R. Merle, Madrapour, copy 1
  , BorrowBook "978-2070367757" 1 -- borrow R. Merle, Week-end a Zuydcoote, copy 1
  , ReturnBook "978-2266230919" 3 -- return R. Barjavel, La nuit des temps, copy 3,
  , ReturnBook "978-2070367757" 1 -- return R. Merle, Week-end a Zuydcoote, copy 1
  , BorrowBook "978-2070401789" 1 -- borrow H. G. Wells, L'ile du docteur Moreau, copy 1
  , AddBook    "978-2070378418" "L'enchanteur" "R. Barjavel" -- add new book
  , BorrowBook "978-2070421206" 1 -- borrow H. P. Lovecraft, Je suis d'ailleurs, copy 1
  , DeleteBook "978-2070367757" 1
  , DeleteBook "978-2290032039" 3
  ]


-- Exercice 1

lookupISBN = undefined

titles = undefined

brList = undefined

countShelvedBooks = undefined

countBorrowedBooks = undefined


-- Exercice 2

lookupTitle = undefined

lookupAuthor = undefined

titleAvailable = undefined

addBook = undefined

emptyLibrary = undefined

mkLibrary = undefined

deleteBook = undefined


-- Exercice 3

borrowBook = undefined

returnBook = undefined


-- Exercice 4

executeScenario = undefined

executeEScenario = undefined


-- Exercice 5

data BookStatus = Shelved | Borrowed deriving (Show)
type BackupRecord = (Book, Serial, BookStatus)
type Backup = [BackupRecord]

backupBookRecord = undefined

backupLibrary = undefined

restoreBook = undefined

restoreLibrary = undefined
