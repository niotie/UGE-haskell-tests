module FileServerTests where

import Test.SmallCheck.Series as SCS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import FileServer

tests =
    testGroup
        "Tests for Tp9"
        [ ex1Tests
        ]

ex1Tests =
    testGroup
        "Tests for exercise 1"
        []
