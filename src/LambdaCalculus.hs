module LambdaCalculus where
import Text.PrettyPrint.ANSI.Leijen

data Expr = 
      Var VariableName
    | Apply Expr Expr
    | Lambda VariableName Expr
    | Constant Literal
    | BuiltInFunc BuiltInFunction
    | Let VariableName Expr Expr
    | Fix Expr
    deriving (Show, Eq, Ord)
    
exprInt = Constant . LInt
    
data Literal = LInt Integer | LString String | Unit deriving (Show, Eq, Ord)

data BuiltInFunction = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

    
type VariableName = String
instance Pretty Expr where
    pretty (Var var) = text var
    pretty (Apply expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Lambda var expr) = char '\\' <+> text var <+> text "->" </> pretty expr
    pretty (Constant l) = pretty l
    pretty (Fix f) = pretty f
    --could be nice to print the greek letter
    
instance Pretty Literal where
    pretty (LInt n) = pretty n
    pretty (LString str) = dquotes $ text str
    pretty Unit = text "()"
    
--(Apply (Lambda "x" (Constant (Lint 3))) (Constant (Lint 2)))

isRecursive :: String -> Expr -> Bool
isRecursive n (Var x) = n == x
isRecursive n (Apply x y) = isRecursive n x || isRecursive n y
isRecursive n (Lambda x expr)
    |n == x = False --Shadowing, ex f1 = (\f2 -> f2...)
    |otherwise = isRecursive n expr
isRecursive n (Let x expr1 expr2) = --f1 = let f2 = f1 a in f2 b
    isRecursive n expr1 || (if n == x then False else isRecursive n expr2)

transformToFixPoint :: String -> Expr -> Expr
transformToFixPoint name expr
    |isRecursive name expr = Fix $ Lambda name expr
    |otherwise = expr
    
