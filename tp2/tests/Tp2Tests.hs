module Tp2Tests where

import Test.SmallCheck.Series as SCS

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Tp2
import Data.List

tests = testGroup "Tests for TP 2"
    [ ex1Tests
    , ex2Tests
    , ex3Tests
    , ex4Tests
    , ex5Tests
    , ex6Tests
    ]

ex1Tests = testGroup "Tests for exercise 1"
    [ headTests
    , tailTests
    , lastTests
    , intervalTests
    , interval'Tests
    ]

headTests = testGroup "Property tests for head'"
    [ SC.testProperty 
        "head' behaves like head on nonempty Int lists (SmallCheck)" $
        \(SCS.NonEmpty l) -> head' (l :: [Int]) == head l
    , QC.testProperty 
        "head' behaves like head on nonempty Int lists (QuickCheck)" $
        \(QC.NonEmpty l) -> head' (l :: [Int]) == head l
    ] 

tailTests = testGroup "Property tests for tail'"
    [ SC.testProperty 
        "tail' behaves like tail on nonempty Int lists (SmallCheck)" $
        \(SCS.NonEmpty l) -> tail' (l :: [Int]) == tail l
    , QC.testProperty 
        "tail' behaves like tail on nonempty Int lists (QuickCheck)" $
        \(QC.NonEmpty l) -> tail' (l :: [Int]) == tail l
    ]

lastTests = testGroup "Property tests for last'"
    [ SC.testProperty 
        "last' behaves like last on nonempty Int lists (SmallCheck)" $
        \(SCS.NonEmpty l) -> last' (l :: [Int]) == last l
    , QC.testProperty 
        "last' behaves like last on nonempty Int lists (QuickCheck)" $
        \(QC.NonEmpty l) -> last' (l :: [Int]) == last l
    ]

intervalTests = testGroup "Property tests for interval"
    [ SC.testProperty "interval n for n >= 0 equals [1..n] (SmallCheck)" $
        \(SCS.NonNegative n) -> interval (n :: Int) == [1..n]
    , QC.testProperty "interval n for n >= 0 equals [1..n] (QuickCheck)" $
        \(QC.NonNegative n) -> interval (n :: Int) == [1..n]
    ] 

interval'Tests = testGroup "Property tests for interval"
    [ SC.testProperty "interval' s k equals [s..k] (SmallCheck)" $
        \s k -> interval' (s :: Int) (k :: Int) == [s..k]
    , QC.testProperty "interval' s k equals [s..k] (QuickCheck)" $
        \s k -> interval' (s :: Int) (k :: Int) === [s..k]
    ] 

ex2Tests = testGroup "Tests for exercise 2"
    [ isPalindromeTest
    , pairsTest
    , prefixesTest
    , factorsTest
    , subseqsTest
    ]

isPalindromeTest = testGroup "Unit tests for reverse'"
    [ testCase "[] is a palindrome" $
        isPalindrome ([] :: [Int]) @?= True
    , testCase "\"radar\" is a palindrome" $
        isPalindrome "radar"  @?= True
    , testCase "\"abba\" is a palindrome" $
        isPalindrome "abba"   @?= True
    , testCase "\"abaa\" is not a palindrome" $
        isPalindrome "abaa"   @?= False
    ]

pairsTest = testGroup "Tests for pairs"
    [ testGroup "Unit tests for pairs"
        [ testCase "pairs [] == []" $
            pairs ([] :: [Int]) @?= ([] :: [(Int, Int)])
        , testCase "pairs [1] == []" $
            pairs [1] @?= ([] :: [(Int, Int)])
        , testCase "pairs [1, 2, 3] == [(1, 2), (2, 3)]" $
            pairs [1, 2, 3] @?= [(1, 2), (2, 3)]
        ]
    , testGroup "Property tests for pairs"
        [ SC.testProperty "projections yield the whole list (SmallCheck)" $
            \(SCS.NonEmpty xs) -> let (ys, zs) = unzip $ pairs (xs :: [Int]) in
                ys ++ [last xs] == xs
        , QC.testProperty "projections yield the whole list (QuickCheck)" $
            \(QC.NonEmpty xs) -> let (ys, zs) = unzip $ pairs (xs :: [Int]) in
                ys ++ [last xs] == xs
        ] 
    ]

factorsTest = testGroup "Tests for factors"
    -- FIXME: not testing for distinctness, no empty factor
    [ testGroup "Unit tests for factors"
        [ testCase "factors [] == []" $
            factors ([] :: [Int]) @?= ([] :: [[Int]])
        , testCase "factors [1] (up to order)" $
            sort (factors [1]) @?= [[1]]
        , testCase "factors [1, 2, 3, 4] (up to order)" $
            sort (factors [1, 2, 3, 4]) @?= 
                [ [1],[1,2],[1,2,3],[1,2,3,4]
                , [2],[2,3],[2,3,4]
                , [3],[3,4]
                , [4]
                ]
        ]
    , testGroup "Property tests for factors"
        [ SC.testProperty "all elements are actual factors (SmallCheck)" $
            \xs -> all (`isInfixOf` xs) (factors (xs :: [Int]) :: [[Int]])
        , SC.testProperty "number of factors is n(n+1)/2 (SmallCheck)" $
            \xs -> let n = length xs; n' = n * (n + 1) `div` 2 in
                length (factors (xs :: [Int]) :: [[Int]]) == n'
        , QC.testProperty "all elements are actual factors (QuickCheck)" $
            \xs -> find (not . (`isInfixOf` xs)) (factors (xs :: [Int]) :: [[Int]]) === Nothing
        , QC.testProperty "number of factors is n(n+1)/2 (QuickCheck)" $
            \xs -> let n = length xs; n' = n * (n + 1) `div` 2 in
                length (factors (xs :: [Int]) :: [[Int]]) === n'
        ] 
    ]

prefixesTest = testGroup "Tests for prefixes"
    -- FIXME: not testing for distinctness, no empty factor
    [ testGroup "Unit tests for prefixes"
        [ testCase "prefixes [] == []" $
            prefixes ([] :: [Int]) @?= ([] :: [[Int]])
        , testCase "prefixes [1] (up to order)" $
            sort (prefixes [1]) @?= [[1]]
        , testCase "prefixes [1, 2, 3, 4] (up to order)" $
            sort (prefixes [1, 2, 3, 4]) @?= 
                [ [1],[1,2],[1,2,3],[1,2,3,4] ]
        ]
    , testGroup "Property tests for prefixes"
        [ SC.testProperty "all elements are actual prefixes (SmallCheck)" $
            \xs -> all (`isInfixOf` xs) (prefixes (xs :: [Int]) :: [[Int]])
        , SC.testProperty "number of prefixes is n (SmallCheck)" $
            \xs -> let n = length xs in
                length (prefixes (xs :: [Int]) :: [[Int]]) == n
        , QC.testProperty "all elements are actual prefixes (QuickCheck)" $
            \xs -> find (not . (`isPrefixOf` xs)) (prefixes (xs :: [Int]) :: [[Int]]) === Nothing
        , QC.testProperty "number of prefixes is n (QuickCheck)" $
            \xs -> let n = length xs in
                length (prefixes (xs :: [Int]) :: [[Int]]) === n
        ] 
    ]

subseqsTest = testGroup "Tests for subseqs"
    [ testGroup "Unit tests for subseqs"
        [ testCase "subseqs [] == []" $
            subseqs ([] :: [Int]) @?= ([[]] :: [[Int]])
        , testCase "subseqs [1] == [[], [1]] (up to order)" $
            sort (subseqs [1]) @?= [[], [1]]
        , testCase "subseqs [1, 2, 3] == [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]] (up to order)" $
            sort (subseqs [1, 2, 3]) @?= [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
        ]
    , testGroup "Property tests for subseqs"
        [ SC.testProperty "subseqs behaves like subsequences (up to order) (SmallCheck)" $
            \xs -> sort (subseqs (xs :: [Int])) == sort (subsequences xs)
        -- Removed (too slow !):
        -- , QC.testProperty "projections yield the whole list (QuickCheck)" $
        --     \xs -> sort (subseqs (xs :: [Int])) === sort (subsequences xs)
        ] 
    ]

ex3Tests = testGroup "Tests for exercise 3"
    [ isArithSerieTest
    , isConstantTest
    , mkArithSerieTest
    , complementaryEx4Test
    ]

isArithSerieTest = testGroup "Tests for isArithSerie"
    [ testGroup "Unit tests for isArithSerie"
        [ testCase "isArithSerie [] == True" $
            isArithSerie [] @=? True
        , testCase "isArithSerie [2] == True" $
            isArithSerie [2] @=? True
        , testCase "isArithSerie [2, 5] == True" $
            isArithSerie [2, 5] @=? True
        , testCase "isArithSerie [2,5,8,11] == True" $
            isArithSerie [2,5,8,11] @=? True
        , testCase "isArithSerie [2,5,7,11] == False" $
            isArithSerie [2,5,7,11] @=? False
        ]
    ]

isConstantTest = testGroup "Tests for isConstant"
    [ testGroup "Unit tests for isConstant"
        [ testCase "isConstant [] == True" $
            isConstant ([] :: [Int]) @=? True
        , testCase "isConstant [2] == True" $
            isConstant [2] @=? True
        , testCase "isConstant [2, 2, 2, 5] == False" $
            isConstant [2, 2, 2, 5] @=? False
        , testCase "isConstant [2, 2, 2, 2] == True" $
            isConstant [2, 2, 2, 2] @=? True
        ]
    , testGroup "Property tests for isConstant"
        [ SC.testProperty 
            "results of replicate make isConstant True (SmallCheck)" $
            \n c -> isConstant $ replicate n (c :: Int) :: Bool
        , QC.testProperty 
            "results of replicate make isConstant True (QuickCheck)" $
            \n c -> isConstant $ replicate n (c :: Int) :: Bool
        ]
    ]

mkArithSerieTest = testGroup "Tests for mkArithSerie"
    [ testGroup "Unit tests for mkArithSerie"
        [ testCase "mkArithSerie 1 1 0 == []" $
            mkArithSerie 1 1 0 @=? ([] :: [Int])
        , testCase "mkArithSerie 1 2 3 == [1, 3, 5]" $
            mkArithSerie 1 2 3 @=? [1, 3, 5]
        , testCase "mkArithSerie 10 5 4 == [10, 15, 20, 25]" $
            mkArithSerie 10 5 4 @=? [10, 15, 20, 25]
        ]
    ]

complementaryEx4Test = testGroup "Complementary property tests for exercise 4"
    [ SC.testProperty 
        "results of mkArithSerie u0 0 _ make isConstant True (SmallCheck)" $
        \u (SCS.NonNegative n) -> 
            isConstant $ mkArithSerie (u :: Int) 0 (n :: Int) :: Bool
    , SC.testProperty 
        "results of mkArithSerie u0 r _ make isArithSerie True (SmallCheck)" $
        \u r (SCS.NonNegative n) -> 
            isArithSerie $ mkArithSerie (u :: Int) (r :: Int) (n :: Int) :: Bool
    , QC.testProperty 
        "results of mkArithSerie u0 0 _ make isConstant True (SmallCheck)" $
        \u (QC.NonNegative n) -> 
            isConstant $ mkArithSerie (u :: Int) 0 (n :: Int) :: Bool
    , QC.testProperty 
        "results of mkArithSerie u0 r _ make isArithSerie True (SmallCheck)" $
        \u r (QC.NonNegative n) -> 
            isArithSerie $ mkArithSerie (u :: Int) (r :: Int) (n :: Int) :: Bool
    ]

ex4Tests = testGroup "Tests for exercise 4"
    [ isEvenTest
    , isOddTest
    , isEvenOddAlternatingTest
    , isOddEvenAlternatingTest
    , isAlternatingTest
    ]

isEvenTest = testGroup "Property tests for isEven"
    [ SC.testProperty 
        "isEven n equals even n (SmallCheck)" $
        \(SCS.NonNegative n) -> isEven (n::Int) == even n
    , QC.testProperty 
        "isEven n equals even n (QuickCheck)" $
        \(QC.NonNegative n) -> isEven (n::Int) === even n
    ]

isOddTest = testGroup "Property tests for isOdd"
    [ SC.testProperty 
        "isOdd n equals odd n (SmallCheck)" $
        \(SCS.NonNegative n) -> isOdd (n::Int) == odd n
    , QC.testProperty 
        "isOdd n equals odd n (SmallCheck)" $
        \(QC.NonNegative n) -> isOdd (n::Int) === odd n
    ]

isEvenOddAlternatingTest = testGroup "Property tests for isEvenOddAlternating"
    [ SC.testProperty 
        "isEvenOddAlternating [m..(m+n)] iff m is even (SmallCheck)" $
        \(SCS.NonNegative m) (SCS.NonNegative n) -> 
            isEvenOddAlternating ([m..m+n] :: [Int]) == even m
    , SC.testProperty 
        "not isEvenOddAlternating (xs ++ [last xs]) (SmallCheck)" $
        \(SCS.NonEmpty xs) -> 
            not $ isEvenOddAlternating (xs ++ [last xs] :: [Int])
    , QC.testProperty 
        "isEvenOddAlternating [m..(m+n)] iff m is even (QuickCheck)" $
        \(QC.NonNegative m) (QC.NonNegative n) -> 
            isEvenOddAlternating ([m..m+n] :: [Int]) == even m
    , QC.testProperty 
        "not isEvenOddAlternating (xs ++ [last xs]) (QuickCheck)" $
        \(QC.NonEmpty xs) -> 
            not $ isEvenOddAlternating (xs ++ [last xs] :: [Int])
    ]

isOddEvenAlternatingTest = testGroup "Property tests for isOddEvenAlternating"
    [ SC.testProperty 
        "isOddEvenAlternating [m..(m+n)] iff m is odd (SmallCheck)" $
        \(SCS.NonNegative m) (SCS.NonNegative n) -> 
            isOddEvenAlternating ([m..m+n] :: [Int]) == odd m
    , SC.testProperty 
        "not isOddEvenAlternating (xs ++ [last xs]) (SmallCheck)" $
        \(SCS.NonEmpty xs) -> 
            not $ isOddEvenAlternating (xs ++ [last xs] :: [Int])
    , QC.testProperty 
        "isOddEvenAlternating [m..(m+n)] iff m is odd (QuickCheck)" $
        \(QC.NonNegative m) (QC.NonNegative n) -> 
            isOddEvenAlternating ([m..m+n] :: [Int]) == odd m
    , QC.testProperty 
        "not isOddEvenAlternating (xs ++ [last xs]) (QuickCheck)" $
        \(QC.NonEmpty xs) -> 
            not $ isOddEvenAlternating (xs ++ [last xs] :: [Int])
    ]

isAlternatingTest = testGroup "Property tests for isAlternating"
    [ SC.testProperty 
        "isAlternating [m..(m+n)] (SmallCheck)" $
        \(SCS.NonNegative m) (SCS.NonNegative n) -> 
            isAlternating ([m..m+n] :: [Int]) :: Bool
    , SC.testProperty 
        "not isAlternating (xs ++ [last xs]) (SmallCheck)" $
        \(SCS.NonEmpty xs) -> 
            not $ isAlternating (xs ++ [last xs] :: [Int])
    , QC.testProperty 
        "isAlternating [m..(m+n)] (QuickCheck)" $
        \(QC.NonNegative m) (QC.NonNegative n) -> 
            isAlternating ([m..m+n] :: [Int]) :: Bool
    , QC.testProperty 
        "not isAlternating (xs ++ [last xs]) (QuickCheck)" $
        \(QC.NonEmpty xs) -> 
            not $ isAlternating (xs ++ [last xs] :: [Int])
    ]

ex5Tests = testGroup "Tests for exercise 5"
    [ mergeMsplitTest
    , mergeSortTest
    , quickSortTest
    ]

mergeMsplitTest = testGroup "Property tests for msplit and merge"
    [ SC.testProperty 
        "uncurry merge $ msplit [0..n] == [0..n] (SmallCheck)" $
        \(SCS.NonNegative n) -> 
            (uncurry merge . msplit) ([0..n] :: [Int]) == [0..n]
    , SC.testProperty 
        "msplit $ merge [0..n] [0..n] == ([0..n], [0..n]) (SmallCheck)" $
        \(SCS.NonNegative n) -> 
            msplit (merge [0..(n::Int)] [0..n]) == ([0..n], [0..n])
    , QC.testProperty 
        "uncurry merge $ msplit [0..n] == [0..n] (SmallCheck)" $
        \(QC.NonNegative n) -> 
            (uncurry merge . msplit) ([0..n] :: [Int]) === [0..n]
    , QC.testProperty 
        "msplit $ merge [0..n] [0..n] == [0..n] (SmallCheck)" $
        \(QC.NonNegative n) -> 
            msplit (merge [0..(n::Int)] [0..n]) === ([0..n], [0..n])
    ]

mergeSortTest = testGroup "Property tests for mergeSort"
    [ SC.testProperty 
        "mergeSort behaves as sort (SmallCheck)" $
        \xs -> mergeSort (xs :: [Int]) == sort xs
    , QC.testProperty 
        "mergeSort behaves as sort (QuickCheck)" $
        \xs -> mergeSort (xs :: [Int]) === sort xs
    ]

quickSortTest = testGroup "Property tests for quickSort"
    [ SC.testProperty 
        "quickSort behaves as sort (SmallCheck)" $
        \xs -> quickSort (xs :: [Int]) == sort xs
    , QC.testProperty 
        "quickSort behaves as sort (QuickCheck)" $
        \xs -> quickSort (xs :: [Int]) === sort xs
    ]

ex6Tests = testGroup "Tests for exercise 6"
    [ sumTest
    , maximumTest
    , lengthTest
    , zipTest
    , takeTest
    , dropTest
    , elemTest
    , reverseTest
    ]

sumTest = testGroup "Property tests for sum'"
    [ SC.testProperty 
        "sum' behaves as sum (SmallCheck)" $
        \xs -> sum' (xs :: [Int]) == sum xs
    , QC.testProperty 
        "sum' behaves as sum (QuickCheck)" $
        \xs -> sum' (xs :: [Int]) === sum xs
    ]

maximumTest = testGroup "Property tests for maximum'"
    [ SC.testProperty 
        "maximum' behaves as maximum (SmallCheck)" $
        \(SCS.NonEmpty xs) -> maximum' (xs :: [Int]) == maximum xs
    , QC.testProperty 
        "maximum' behaves as maximum (QuickCheck)" $
        \(QC.NonEmpty xs) -> maximum' (xs :: [Int]) === maximum xs
    ]

lengthTest = testGroup "Property tests for length'"
    [ SC.testProperty 
        "length' behaves as length (SmallCheck)" $
        \xs -> length' (xs :: [Int]) == length xs
    , QC.testProperty 
        "length' behaves as length (QuickCheck)" $
        \xs -> length' (xs :: [Int]) === length xs
    ]

zipTest = testGroup "Property tests for zip'"
    [
    -- too slow!
    -- SC.testProperty 
    --     "zip' behaves as zip (SmallCheck)" $
    --     \xs ys -> zip' (xs :: [Int]) (ys :: [Int]) == zip xs ys
    -- , 
    QC.testProperty 
        "zip' behaves as zip (QuickCheck)" $
        \xs ys -> zip' (xs :: [Int]) (ys :: [Int]) === zip xs ys
    ]

takeTest = testGroup "Property tests for take'"
    [ SC.testProperty 
        "take' behaves as take (SmallCheck)" $
        \(SCS.NonNegative n) xs -> take' n (xs :: [Int]) == take n xs
    , QC.testProperty 
        "take' behaves as take (QuickCheck)" $
        \(QC.NonNegative n) xs -> take' n (xs :: [Int]) === take n xs
    ]

dropTest = testGroup "Property tests for drop'"
    [ SC.testProperty 
        "drop' behaves as drop (SmallCheck)" $
        \(SCS.NonNegative n) xs -> drop' n (xs :: [Int]) == drop n xs
    , QC.testProperty 
        "drop' behaves as drop (QuickCheck)" $
        \(QC.NonNegative n) xs -> drop' n (xs :: [Int]) === drop n xs
    ]

elemTest = testGroup "Property tests for elem'"
    [ SC.testProperty 
        "elem' behaves as elem (SmallCheck)" $
        \n xs -> elem' n (xs :: [Int]) == elem n xs
    , QC.testProperty 
        "elem' behaves as elem (QuickCheck)" $
        \n xs -> elem' n (xs :: [Int]) === elem n xs
    ]

reverseTest = testGroup "Property tests for reverse'"
    [ SC.testProperty "reverse' xs equals reverse xs (SmallCheck)" $
        \xs -> reverse' (xs :: [Int]) == reverse xs
    , QC.testProperty "reverse' xs equals reverse xs (QuickCheck)" $
        \xs -> reverse' (xs :: [Int]) === reverse xs
    ]
