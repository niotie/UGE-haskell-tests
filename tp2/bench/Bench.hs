import Criterion.Main
import Tp2

xs100 = [1..100]
xs10 = [1..10]

-- Our benchmark harness.
main = defaultMain [
    bgroup "subseqs" [ bench ((show (n :: Int)) ++ " elements") $ nf (subseqs :: [Int] -> [[Int]]) [1..n] | 
                       n <- [1..15]]
  ]