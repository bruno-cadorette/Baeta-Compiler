{-# LANGUAGE DeriveFunctor #-}
module LambdaCalculus where
import Text.PrettyPrint.ANSI.Leijen

type NonTypedExpr = Expr ()
type Variable a = Named a
data Named a = Named { getName :: String, getValue :: a } deriving (Show, Eq, Ord, Functor)
class ExprValue a where
    getAppValue :: Expr a -> Expr a -> a
    getLambdaValue :: Variable a -> Expr a -> a
    
instance ExprValue () where
    getAppValue _ _ = ()
    getLambdaValue _ _ = ()

data Expr a = 
      Var (Variable a)
    | Apply (Expr a) (Expr a)
    | Lambda (Variable a) (Expr a)
    | Constant Literal a
    -- | Let (Variable a) (Expr a) (Expr a)
    | Fix (Variable a) (Expr a)
    deriving (Show, Eq, Ord)
    
getExprValue (Var x) = getValue x
getExprValue (Apply expr1 expr2) = getAppValue expr1 expr2
getExprValue (Lambda x expr) = getLambdaValue x expr
getExprValue (Constant l x) = x
getExprValue (Fix x expr) = getExprValue expr
    
nonTypedVar str = Named str ()
    
exprInt = Constant . LInt
    
data Literal = LInt Integer | LString String | Unit deriving (Show, Eq, Ord)

instance Pretty (Named a) where
    pretty (Named a _) = text a

instance Pretty (Expr a) where
    pretty (Var var) = text $ getName var
    pretty (Apply expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Lambda var expr) = char '\\' <+> pretty var <+> text "->" </> pretty expr
    pretty (Constant l _) = pretty l
    pretty (Fix x f) = pretty f
    --could be nice to print the greek letter
    
instance Pretty Literal where
    pretty (LInt n) = pretty n
    pretty (LString str) = dquotes $ text str
    pretty Unit = text "()"
    
--(Apply (Lambda "x" (Constant (Lint 3))) (Constant (Lint 2)))

isRecursive :: String -> Expr a -> Bool
isRecursive n (Var x) = n == getName x
isRecursive n (Apply x y) = isRecursive n x || isRecursive n y
isRecursive n (Lambda x expr)
    |n == getName x = False --Shadowing, ex f1 = (\f2 -> f2...)
    |otherwise = isRecursive n expr
--isRecursive n (Let x expr1 expr2) = --f1 = let f2 = f1 a in f2 b
--    isRecursive n expr1 || (if n == getName x then False else isRecursive n expr2)

transformToFixPoint :: String -> NonTypedExpr -> NonTypedExpr
transformToFixPoint name expr
    |isRecursive name expr = Fix (nonTypedVar name) expr
    |otherwise = expr
    
