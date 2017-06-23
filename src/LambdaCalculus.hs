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
    
{-
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
    
   -}
    
hoist :: (m a -> n a) -> LambdaExpr m a -> LambdaExpr n a
hoist f (Var a) = Var a
hoist f (Apply a b) = Apply (f a) (f b)
hoist f (Lambda a b) = Lambda a (f b)

    
data LambdaExpr m a = 
    Var (Named a) | Apply (m a) (m a) | Lambda (Named a) (m a)
    deriving (Show, Eq, Functor)
    
newtype LambdaCalculus a = LambdaCalculus (LambdaExpr LambdaCalculus a) deriving (Show, Eq, Functor)
    
data Expr a = 
    LC (LambdaExpr Expr a) 
    | Constant Literal a 
    | Fix String (Expr a)
    | If (Expr a) (Expr a) (Expr a)
    deriving (Show, Eq, Functor)



instance Show Func where
    show f = "Literal -> Literal"
    
instance Eq Func where
    a == b = False
    
nonTypedVar str = Named str ()
    
exprInt = Constant . LInt
    
    

instance Pretty a => Pretty (Named a) where
    pretty (Named a b) = text a <+> text ":" <+> parens (pretty b)
    
instance (Pretty (f a), Pretty a) => Pretty (LambdaExpr f a) where
    pretty (Var var) = pretty var
    pretty (Apply expr1 expr2) = parens (pretty expr1) <+> parens (pretty expr2)
    pretty (Lambda var expr) = char '\\' <> pretty var <+> text "->" <+> parens (pretty expr)

instance Pretty a => Pretty (Expr a) where
    pretty (LC lambda) = pretty lambda
    pretty (Constant l b) = pretty l <+> text ":" <+> pretty b
    pretty (Fix n f) = text ("FixPoint[" ++ n ++ "]") <+> parens (pretty f)
    pretty (If c a b) = text "if " <+> parens (pretty c) <+> parens (pretty a) <+> parens (pretty b) 
    
instance Pretty Literal where
    pretty (LInt n) = pretty n
    pretty (LString str) = dquotes $ text str
    pretty Unit = text "()"
    pretty (LBool b) = text $ show b
    
    
-- |Vérifie si une expression est recursive
isRecursive :: String -> Expr a -> Bool
isRecursive n (LC (Var x)) = n == getName x
isRecursive n (LC (Apply x y)) = isRecursive n x || isRecursive n y
isRecursive n (LC (Lambda x expr))
    |n == getName x = False --Shadowing, ex f1 = (\f2 -> f2...)
    |otherwise = isRecursive n expr
isRecursive n (If c a b) = isRecursive n c || isRecursive n a || isRecursive n b
isRecursive n (Constant l _) = False
isRecursive n (Fix _ expr) = isRecursive n expr

-- |Transforme une expression en fix point si il y a lieu
transformToFixPoint :: String -> NonTypedExpr -> NonTypedExpr
transformToFixPoint name expr
    |isRecursive name expr = Fix name expr
    |otherwise = expr
    
