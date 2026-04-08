module Tp7 where

-- Inner type
type Value = Int

-- Arithmetic expression
data AExpr = Num Value | App BOp AExpr AExpr

-- Binary operator
data BOp = Add | Sub | Mul | Div

-- Computation (i.e., valued arithmetic expression)
newtype VAExpr = VAExpr (AExpr,Value)

instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "x"
  show Div = "/"

instance Show AExpr where
  show (Num x) = show x
  show (App op e1 e2) = "("      ++
                        show e1  ++
                        " "      ++
                        show op  ++
                         " "     ++
                        show e2  ++
                        ")"
                        
instance Show VAExpr where
  show (VAExpr (e,v)) = show e ++ " = " ++ show v


-- Exercice 1 -- Échauffement

sublists = undefined

legal1 = undefined

apply = undefined

value = undefined


-- Exercice 2 -- Un premier solveur

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys')
  | x <= y = x : merge xs' ys
  | otherwise = y : merge xs ys'

unmerges1 = undefined

combineVAExprs1 = undefined

mkAExprs1 = undefined

searchBest = undefined

countdown1 = undefined


-- Exercice 3 -- Un solveur vitaminé

legal2 = undefined

unmerges2 = undefined

combineVAExprs2 = undefined

countdown2 = undefined


-- Exercice 4 -- Pour aller plus loin

countDownAll = undefined

sizedSublists = undefined

sublistsShortestFirst = undefined

sublistsLongestFirst = undefined

countdownTrace = undefined