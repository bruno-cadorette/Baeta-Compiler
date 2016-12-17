{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
-- | Ce module contient l'expression en lambda calcul ainsi que des helpers
module LambdaCalculus where
import Text.PrettyPrint.ANSI.Leijen
type NonTypedExpr = Expr ()
type Variable a = Named a
-- |Représente une valeur qui a un nom
data Named a = Named { getName :: String, getValue :: a } deriving (Show, Eq, Ord, Functor)

-- |Lien entre les fonctions Haskell et Baeta
newtype Func = Func (Literal -> Literal)

-- |Litéraux
data Literal = LInt Integer | LString String | Unit | LBool Bool | BuiltInFunc Func deriving (Show, Eq)
    

-- |L'expression de base. Est polymorphique pour ajouter des valeurs tel que les closures ou bien les types
data Expr a = 
    -- |Variable
      Var (Variable a)
    -- |Application
    | Apply (Expr a) (Expr a)
    -- |Abstraction
    | Lambda (Variable a) (Expr a) 
    -- |Constante
    | Constant Literal a 
    -- |Fix point, utilisé seulement pour l'inférence de type. Devrait peut-être être sorti de cette structure
    | Fix String (Expr a) 
     -- |La fonction if
    | If (Expr a) (Expr a) (Expr a) 
    deriving (Show, Eq, Functor)
    
instance Show Func where
    show f = "Literal -> Literal"
    
instance Eq Func where
    a == b = False
    
nonTypedVar str = Named str ()
    
exprInt = Constant . LInt
    


instance Pretty (Named a) where
    pretty (Named a _) = text a

instance Pretty (Expr a) where
    pretty (Var var) = text $ getName var
    pretty (Apply expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Lambda var expr) = char '\\' <+> pretty var <+> text "->" </> pretty expr
    pretty (Constant l _) = pretty l
    pretty (Fix n f) = pretty f
    pretty (If c a b) = text "if " <> pretty c <> pretty a <> pretty b 
    
instance Pretty Literal where
    pretty (LInt n) = pretty n
    pretty (LString str) = dquotes $ text str
    pretty Unit = text "()"
    pretty (LBool b) = text $ show b
    
    
-- |Vérifie si une expression est recursive
isRecursive :: String -> Expr a -> Bool
isRecursive n (Var x) = n == getName x
isRecursive n (Apply x y) = isRecursive n x || isRecursive n y
isRecursive n (Constant l _) = False
isRecursive n (Fix _ expr) = isRecursive n expr
isRecursive n (Lambda x expr)
    |n == getName x = False --Shadowing, ex f1 = (\f2 -> f2...)
    |otherwise = isRecursive n expr
isRecursive n (If c a b) = isRecursive n c || isRecursive n a || isRecursive n b

-- |Transforme une expression en fix point si il y a lieu
transformToFixPoint :: String -> NonTypedExpr -> NonTypedExpr
transformToFixPoint name expr
    |isRecursive name expr = Fix name expr
    |otherwise = expr
    
