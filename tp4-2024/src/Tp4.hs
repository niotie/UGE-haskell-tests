module Tp4 where

spinWords = undefined

unshuffle = undefined

algoA = undefined

encode = undefined

shuffle = undefined

revAlgoA = undefined

decode = undefined

easyCrack :: Eq a => [a] -> [[a]]
easyCrack xs = xs : f xss
  where
    xss = iterate (flip encode 1) xs
    f = takeWhile (/= xs) . tail
