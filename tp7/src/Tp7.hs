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
  
