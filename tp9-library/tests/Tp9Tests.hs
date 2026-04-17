module Tp9Tests where

import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
-- import Test.SmallCheck.Series as SCS

import Tp9

tests =
  testGroup
    "Tests for Tp9"
    [ ex1Tests -- , ex2Tests
    ]

ex1Tests =
  testGroup
    "Tests for exercise 1"
    [ q1aTests
    , q1bTests
    , q1cTests
    , q1dTests
    ]

q1aTests =
  testGroup
    "Tests for question a"
    [ testCase "lookupISBN \"978-2266230919\" myLib" $
        lookupISBN "978-2266230919" myLib
          @?= Just (Book "978-2266230919" "La nuit des temps" "R. Barjavel", 1, 2)
    , testCase "lookupISBN \"978-2070367757\" myLib" $
        lookupISBN "978-2070367757" myLib
          @?= Just (Book "978-2070367757" "Week-end a Zuydcoote" "R. Merle", 1, 0)
    , testCase "lookupISBN \"000-0000000000\" myLib" $
        lookupISBN "000-0000000000" myLib
          @?= Nothing
    ]

q1bTests =
  testGroup
    "Tests for question b"
    [ testCase "titles myLib" $
        titles myLib
          @?= [ ("978-2020336512", "Madrapour")
              , ("978-2070362387", "Ravage")
              , ("978-2070364855", "Le voyageur imprudent")
              , ("978-2070367757", "Week-end a Zuydcoote")
              , ("978-2070374441", "Malevil")
              , ("978-2070401789", "L'ile du docteur Moreau")
              , ("978-2070415809", "La couleur tombee du ciel")
              , ("978-2070421206", "Je suis d'ailleurs")
              , ("978-2070782109", "La machine a explorer le temps")
              , ("978-2266230919", "La nuit des temps")
              , ("978-2290032039", "La carte et le territoire")
              , ("978-2919695010", "La carte et le territoire")
              ]
    ]

q1cTests =
  testGroup
    "Tests for question c"
    [ testCase "countShelvedBooks myLib" $
        countShelvedBooks myLib
          @?= 11
    ]

q1dTests =
  testGroup
    "Tests for question d"
    [ testCase "countBorrowedBooks myLib" $
        countBorrowedBooks myLib
          @?= 5
    ]

