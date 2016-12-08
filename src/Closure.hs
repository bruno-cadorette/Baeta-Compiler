{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Closure where
import LambdaCalculus
import qualified Data.Map as Map

newtype Environment a = Environment [Variable a] deriving (Show, Monoid)
addEnv x (Environment xs) = Environment (x:xs)
    
includeEnv env = fmap (\x -> (x, env)) 
    
closure :: Environment a -> Expr a -> Expr (a, Environment a)
closure env (Var a) = Var $ includeEnv env a
closure env (Apply expr1 expr2) = Apply (closure env expr1) (closure env expr2)
closure env (Lambda x expr) = Lambda (includeEnv env x) (closure (addEnv x env) expr)
closure env (Constant l t) = Constant l (t, mempty)
closure env (Fix x expr) = Fix (includeEnv env x) (closure (addEnv x env) expr)

allClosure = map (closure mempty)

toVar x = Named x ()

t = (Lambda (toVar "x") (Lambda (toVar "y") (Lambda (toVar "z") (Apply (Var $ toVar "z") (Apply (Var $ toVar "x") (Var $ toVar "y"))))))

t2 = (Lambda (toVar "x") (Lambda (toVar "y") (Var (toVar "x"))))
t3 = (Lambda (toVar "y") (Var (toVar "x")))
    
--f1 = Lambda "x" (Lambda "y" (Apply (Var "y") (Var "x")))
    
-- (\x. \y. y x) (2 + 2) (\x. x + 1)
-- (\x . \{x} y . (x y)
--example =  Apply (Apply f1 f2) f3