module LambdaCalculus where

data Expr = 
      Var VariableName
    | Apply Expr Expr
    | Lambda VariableName Expr
    deriving (Show, Eq, Ord)
    
type VariableName = String
