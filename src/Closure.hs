{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
-- |Module permettant de faire la gestions des closures
module Closure where
import LambdaCalculus
import Data.List
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Map as Map

newtype Environment a = Environment [Variable a] deriving (Show, Monoid, Functor, Eq)
instance Pretty a => Pretty (Environment a) where
    pretty (Environment xs) = cat $ punctuate comma $ fmap (text . getName) xs
    
    
-- | Ajoute une variable a un environment
addEnv x (Environment xs) = Environment (x:xs)
-- | Expression ayant des closures (est récursif, car les closures peuvent elles même avoir des expressions avec closure)
data ExprEval = ExprEval ExprEnv deriving (Show, Eq)
type ExprEnv = Expr (Environment (Maybe ExprEval))

instance Pretty ExprEval where
    pretty (ExprEval a) = pretty a

toEvalType :: Expr (a, Environment a) -> ExprEnv
toEvalType = fmap (fmap (const Nothing) . snd)

-- | Update une valeur de l'environment
updateEnvValue n v (Environment env) = Environment (replace env)
    where
        replace [] = []
        replace (x:xs)
            |n == getName x = fmap (const v) x : xs
            |otherwise = x : replace xs
            
-- | Obtient la valeur d'un environment
getEnvValue n (Environment env) = getValue <$> find (\v -> getName v == n) env
    
    
includeEnv env = fmap (\x -> (x, env)) 
    
-- | Ajoute un environment a une expression
closure :: Environment a -> Expr a -> Expr (a, Environment a)
closure env (LC (Var a)) = LC $ Var $ includeEnv env a
closure env (LC (Apply expr1 expr2)) = LC $ Apply (closure env expr1) (closure env expr2)
closure env (LC (Lambda x expr)) = LC $ Lambda (includeEnv env x) (closure (addEnv x env) expr)
closure env (Constant l t) = Constant l (t, mempty)
closure env (Fix n expr) = Fix n (closure env expr)
closure env (If c a b) = If (closure env c) (closure env a) (closure env b)

-- | Ajoute un environment a une expression, pas d'environment initial
addClosure = closure mempty

toVar x = Named x ()