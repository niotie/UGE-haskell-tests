module Tp3 where


-- Exercise 1 -----------------

elem' = undefined

map1 = undefined

map2 = undefined

map3 = undefined

filter1 = undefined

filter2 = undefined

filter3 = undefined

takeWhile1 = undefined

takeWhile2 = undefined

dropWhile1 = undefined

dropWhile2 = undefined

zipWith1 = undefined

zipWith2 = undefined

reverse1 = undefined

reverse2 = undefined

reverse3 = undefined


-- Exercise 2 -----------------

fibonacciSeq1 :: [Integer]
fibonacciSeq1 = [f n | n <- [0..]]
    where
        f 0 = 0
        f 1 = 1
        f n = f (n-1) + f (n-2)

fibonacciSeq2 = undefined

fibonacciSeq3 = undefined

fibonacciSeq4 = 0:zipWith (+) (1:fibonacciSeq4) fibonacciSeq4


-- Exercise 3 -----------------

distrib = undefined

permutations1 = undefined

shuffles = undefined

permutations2 = undefined

permutations3 = undefined


-- Exercise 4 -----------------

unfold p h t x
    | p x = []
    | otherwise = h x:unfold p h t (t x)

map' = undefined

iterate' = undefined

altMap = undefined

digits = undefined

luhn = undefined


-- Exercise 5 ----

gen3 = undefined

gen3' = undefined

magicSquare3 = undefined

normalMagicSquare3 = undefined
