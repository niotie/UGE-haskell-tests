module Tp7Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Tp7

tests = testGroup "Tests for TP 7"
    [ ex1Tests
    -- , ex2Tests
    -- , ex3Tests
    -- , ex4Tests
    -- , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1 -- Échauffement"
    [ 
    ]
