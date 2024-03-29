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

merge = undefined

unmerges1 = undefined

combineVAExprs1 = undefined

mkAExpres1 = undefined

searchBest = undefined

countdown1 = undefined


-- Exercice 3 -- Un solveur vitaminé

legal2 = undefined

combineVAExprs2 :: VAExpr -> VAExpr -> [VAExpr]
combineVAExprs2 (VAExpr (e1,v1)) (VAExpr (e2,v2))
    | v1 < v2 = combineVAExprsUnbalance2 (VAExpr (e1,v1)) (VAExpr (e2,v2))
    | v1 == v2 = combineVAExprsBalance2 (VAExpr (e1,v1)) (VAExpr (e2,v2))
    | otherwise = combineVAExprsUnbalance2 (VAExpr (e2,v2)) (VAExpr (e1,v1))

combineVAExprsBalance2 = undefined

combineVAExprsUnbalance2 = undefined