module Tp3Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCS

import Tp3
import Data.List
    ( delete, isSubsequenceOf, nub, permutations, sort )
import Math.Combinatorics.Exact.Binomial as B (choose)
import qualified Test.SmallCheck.Series as SC
import Data.Char (digitToInt)

tests = testGroup "Tests for TP 3"
    [ ex1Tests
    , ex2Tests
    , ex3Tests
    , ex4Tests
    -- , ex5Tests
    ]

ex1Tests = testGroup "Tests for exercise 1"
    [ elem'Tests
    , mapTests map1 "map1"
    , mapTests map2 "map2"
    , mapTests map3 "map3"
    , filterTests filter1 "filter1"
    , filterTests filter2 "filter2"
    , filterTests filter3 "filter3"
    , takeWhileTests takeWhile1 "takeWhile1"
    , takeWhileTests takeWhile2 "takeWhile2"
    , dropWhileTests dropWhile1 "dropWhile1"
    , dropWhileTests dropWhile2 "dropWhile2"
    , zipWithTests zipWith1 "zipWith1"
    , zipWithTests zipWith2 "zipWith2"
    , reverseTests reverse1 "reverse1"
    , reverseTests reverse2 "reverse2"
    , reverseTests reverse3 "reverse3"
    ]

elem'Tests = testGroup "Tests for elem'"
    [ SC.testProperty 
        "elem' behaves like elem on Int lists (SmallCheck)" $
        \e l -> elem' e (l :: [Int]) == elem e l
    , QC.testProperty 
        "elem' behaves like elem on Int lists (QuickCheck)" $
        \e l -> elem' e (l :: [Int]) === elem e l
    ] 

mapTests mapx mapxname = testGroup ("Tests for " ++ mapxname)
    [ SC.testProperty
        (mapxname ++ 
            " (^2) behaves like map (^2) on Int lists (SmallCheck)") $
        \l -> mapx (^2) (l :: [Int]) == map (^2) l
    , QC.testProperty 
        (mapxname ++ 
            " p behaves like map p on Int lists (QuickCheck)") $
        \p l -> mapx p (l :: [Int]) === map p l
    , testCase ("take 5 $ " ++ mapxname ++ " (^2) [1..] works") $
        take 5 (mapx (^2) [1..]) @?= [1, 4, 9, 16, 25]
    ] 

filterTests filterX filterXName = testGroup ("Tests for " ++ filterXName)
    [ SC.testProperty (filterXName ++ 
            " odd behaves like filter on Int lists (SmallCheck)") $ 
        \l -> filterX odd (l :: [Int]) == filter odd l
    , SC.testProperty (filterXName ++ 
            " ((==0) . (`mod` 3)) behaves like filter on Int lists (SmallCheck)") $ 
        \l -> filterX ((==0) . (`mod` 3)) (l :: [Int]) 
                 == filter ((==0) . (`mod` 3)) l
    , QC.testProperty 
        (filterXName ++ 
            " p behaves like filter p on Int lists for random p (QuickCheck)") $
        \p l -> filterX (p :: Int -> Bool) (l :: [Int]) 
                  === filter p l
    ] 

takeWhileTests takeWhileX takeWhileXName = testGroup ("Tests for " ++ takeWhileXName)
    [ SC.testProperty (takeWhileXName ++ 
            " odd behaves like takeWhile on Int lists (SmallCheck)") $ 
        \l -> takeWhileX odd (l :: [Int]) == takeWhile odd l
    , SC.testProperty (takeWhileXName ++ 
            " ((==0) . (`mod` 3)) behaves like takeWhile on Int lists (SmallCheck)") $ 
        \l -> takeWhileX ((==0) . (`mod` 3)) (l :: [Int]) 
                 == takeWhile ((==0) . (`mod` 3)) l
    , QC.testProperty 
        (takeWhileXName ++ 
            " p behaves like takeWhile p on Int lists for random p (QuickCheck)") $
        \p l -> takeWhileX (p :: Int -> Bool) (l :: [Int]) 
                  === takeWhile p l
    ] 

dropWhileTests dropWhileX dropWhileXName = testGroup ("Tests for " ++ dropWhileXName)
    [ SC.testProperty (dropWhileXName ++ 
            " odd behaves like dropWhile on Int lists (SmallCheck)") $ 
        \l -> dropWhileX odd (l :: [Int]) == dropWhile odd l
    , SC.testProperty (dropWhileXName ++ 
            " ((==0) . (`mod` 3)) behaves like dropWhile on Int lists (SmallCheck)") $ 
        \l -> dropWhileX ((==0) . (`mod` 3)) (l :: [Int]) 
                 == dropWhile ((==0) . (`mod` 3)) l
    , QC.testProperty 
        (dropWhileXName ++ 
            " p behaves like dropWhile p on Int lists for random p (QuickCheck)") $
        \p l -> dropWhileX (p :: Int -> Bool) (l :: [Int]) 
                  === dropWhile p l
    ] 

zipWithTests zipWithX zipWithXName = testGroup ("Tests for " ++ zipWithXName)
    [ SC.testProperty (zipWithXName ++ 
            " (*) behaves like zip on Int lists (SmallCheck)") $ 
        SC.changeDepth (const 3) $
        \l l' -> zipWithX ((*) :: Int -> Int -> Int) (l :: [Int]) (l' :: [Int]) 
                 == zipWith (*) l l'
    , SC.testProperty (zipWithXName ++ 
            " (+) behaves like zipWith (+) on Int lists (SmallCheck)") $ 
        SC.changeDepth (const 3) $
        \l l' -> zipWithX ((+) :: Int -> Int -> Int) (l :: [Int]) (l' :: [Int]) 
                 == zipWith (+) l l'
    , QC.testProperty 
        (zipWithXName ++ 
            " p behaves like zipWith p on Int lists for random p (QuickCheck)") $
        \f l l' -> zipWithX (f :: Int -> Int -> Int) (l :: [Int]) (l' :: [Int]) 
                  === zipWith f l l'
    ] 

reverseTests reversex reversexname = testGroup ("Tests for " ++ reversexname)
    [ SC.testProperty
        (reversexname ++ 
            " behaves like reverse on Int lists (SmallCheck)") $
        \l -> reversex (l :: [Int]) == reverse l
    , QC.testProperty 
        (reversexname ++ 
            " behaves like reverse on Int lists (QuickCheck)") $
        \l -> reversex (l :: [Int]) === reverse l
    ]

ex2Tests = testGroup "Tests for exercise 2"
    [ fibonacciSeqTests fibonacciSeq2 "fibonacciSeq2"
    , fibonacciSeqTests fibonacciSeq3 "fibonacciSeq3"
    ]

fibonacciSeqTests fibonacciSeqx fibonacciSeqxname = testGroup 
    ("Tests for " ++ fibonacciSeqxname)
    [ testCase ("take 10 $ " ++ fibonacciSeqxname ++ " == [0,1,1,2,3,5,8,13,21,34]") $
        take 10 fibonacciSeqx @?= [0,1,1,2,3,5,8,13,21,34]
    , testCase ("sum (take 100 $ " ++ fibonacciSeqxname ++ ") == 573147844013817084100") $
        sum (take 100 fibonacciSeqx) @?= 573147844013817084100
    ]

ex3Tests = testGroup "Tests for exercise 3"
    [ distribTests
    , permutationsTests permutations1 "permutations1"
    , shufflesTests
    , permutationsTests permutations2 "permutations2"
    , permutationsTests permutations3 "permutations3"
    ]

distribTests = testGroup "Tests for distrib"
    [ SC.testProperty
        "all elements of distrib 0 [2..n] are distinct (SmallCheck)" $
        \(SC.Positive n) -> nub (distrib 0 [1..(n::Int)]) == (distrib 0 [1..n] :: [[Int]])
    , QC.testProperty 
        "all elements of distrib 0 [1..n] are distinct (SmallCheck)" $
        \(QC.NonNegative n) -> nub (distrib 0 [1..(n::Int)]) === (distrib 0 [1..n] :: [[Int]])
    , SC.testProperty
        "deleting 0 from distrib 0 [1..n] yields n copies of [1..n] (SmallCheck)" $
        \(SC.NonNegative n) -> map (delete 0) (distrib 0 [1..(n::Int)]) == replicate (n + 1) [1..n]
    , QC.testProperty 
        "deleting 0 from distrib 0 [1..n] yields n copies of [1..n] (SmallCheck)" $
        \(QC.NonNegative n) -> map (delete 0) (distrib 0 [1..(n::Int)]) === replicate (n + 1) [1..n]
    ]

shufflesTests = testGroup "Tests for shuffles"
    [ SC.testProperty
        "elements of l retain their order in shuffles l l' (SmallCheck)" $
        SC.changeDepth (min 4) $
        \l l' -> all (isSubsequenceOf l) (shuffles (l :: [Int]) (l' :: [Int]) :: [[Int]])
    , QC.testProperty 
        "elements of l retain their order in shuffles l l' (QuickCheck)" $
        QC.mapSize (min 4) $
        \l l' -> all (isSubsequenceOf l) (shuffles (l :: [Int]) (l' :: [Int]) :: [[Int]])
    , SC.testProperty
        "elements of l' retain their order in shuffles l l' (SmallCheck)" $
        SC.changeDepth (min 3) $
        \l l' -> all (isSubsequenceOf l') (shuffles (l :: [Int]) (l' :: [Int]) :: [[Int]])
    , QC.testProperty 
        "elements of l' retain their order in shuffles l l'  (QuickCheck)" $
        QC.mapSize (min 4) $
        \l l' -> all (isSubsequenceOf l') (shuffles (l :: [Int]) (l' :: [Int]) :: [[Int]])
    , SC.testProperty
        "all elements of shuffles [1..n] [n+1..n+m] are distinct (SmallCheck)" $
        \(SC.NonNegative n) (SC.NonNegative m) -> 
            let res = shuffles [1..(n::Int)] [n+1..n+m] in
                nub (res :: [[Int]]) == res
    , QC.testProperty 
        "all elements of shuffles [1..n] [n+1..n+m] are distinct  (QuickCheck)" $
        QC.mapSize (min 6) $
        \(QC.NonNegative n) (QC.NonNegative m) -> 
            let res = shuffles [1..(n::Int)] [n+1..n+m] in
                nub (res :: [[Int]]) === res    
    , SC.testProperty
        "all in shuffles l l' contain the same ets as l ++ l' (SmallCheck)" $
        SC.changeDepth (min 4) $
        \l l' -> map sort (shuffles (l :: [Int]) (l' :: [Int])) 
            == replicate (n l l') (sort (l ++ l'))
    , QC.testProperty 
        "all in shuffles l l' contain the same ets as l ++ l' (SmallCheck)" $
        QC.mapSize (min 4) $
        \l l' -> map sort (shuffles (l :: [Int]) (l' :: [Int])) 
            === replicate (n l l') (sort (l ++ l'))
    ] where n l l' = length (l ++ l') `B.choose` length l'

permutationsTests f fname = testGroup 
    ("Tests for " ++ fname)
    [ SC.testProperty
        (fname ++ " behaves like permutations on Int lists, up to sorting (SmallCheck)") $
        \l -> sort (f (l :: [Int])) == sort (permutations l)
    , QC.testProperty 
        (fname ++ " behaves like permutations on Int lists, up to sorting (QuickCheck)") $
        QC.mapSize (min 7) $
        \l -> sort (f (l :: [Int])) === sort (permutations l)
    ]

ex4Tests = testGroup "Tests for exercise 4"
    [ mapTests map' "map'"
    , iterate'Tests (+ 1) "(+ 1)"
    , iterate'Tests (* (-2)) "(* (-2))"
    , altMapTests
    , digitsTests
    , luhnTests
    ]

iterate'Tests f fname = testGroup ("Tests for iterate' " ++ fname)
    [ SC.testProperty
        ("iterate' " ++ fname ++ " behaves like iterate (SmallCheck)") $
        \n -> take 100 (iterate' f (n::Int)) == take 100 (iterate f n)
    , QC.testProperty 
        ("iterate' " ++ fname ++ " behaves like iterate (QuickCheck)") $
        \n -> take 100 (iterate' f n) === take 100 (iterate f n)
    ]

altMapTests = testGroup "Tests for altMap"
    [ testCase "altMap (+10) (*100) [1,2,3,4,5]" $
        altMap (+10) (*100) [1,2,3,4,5] @=? [11,200,13,400,15]
    , testCase "altMap (+10) id [1,2,3,4,5]" $
        altMap (+10) id [1,2,3,4,5] @=? [11,2,13,4,15]
    , testCase "altMap id (*100) [1,2,3,4,5]" $
        altMap id (*100) [1,2,3,4,5] @=? [1,200,3,400,5]
    , testCase "altMap id id [1,2,3,4,5]" $
        altMap id id [1,2,3,4,5] @=? [1,2,3,4,5]
    ]

digitsTests = testGroup "Tests for digits"
    [ SC.testProperty
        "digits of various numbers (SmallCheck)" $
        \(SC.NonNegative n) -> digits (n::Int) == map digitToInt (show n)
    , QC.testProperty 
        "digits of various numbers (QuickCheck)" $
        \(QC.NonNegative n) -> digits (n::Int) === map digitToInt (show n)
    ]

luhnTests = testGroup 
    "Tests for luhn"
    [ testCase "[4,6,2,4, 7,4,8,2, 3,3,2,4, 9,0,8,0] is not valid" $
        luhn [4,6,2,4, 7,4,8,2, 3,3,2,4, 9,0,8,0] @=? False
    , testCase "[4,6,2,4, 7,4,8,2, 3,3,2,4, 9,0,8,0] is valid" $
        luhn [4,6,2,4, 7,4,8,2, 3,3,2,4, 9,7,8,0] @=? True
    ]
