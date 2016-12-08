module TypeInference.Rules where

import TypeInference.Base
import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeInference.TypeVariable
import Control.Monad.State
import LambdaCalculus
import Data.Monoid
import Control.Monad.Trans.Except

type TypedExpr = Expr Monotype

newVariable :: Monad m => Inference m Monotype
newVariable = fmap TVar newTypeVariable

constantType Unit = TConstant "()"
constantType (LInt _) = TConstant "Int"
constantType (LString _) = TConstant "String"

identityType t = createArrow t t

insert key val (Environment env) = Environment $ Map.insert key val env 

--generalize(Γ, τ ) =def ∀α.τ ~ where α = freevars(τ ) − freevars(Γ)

generalize :: Environment -> Monotype -> Polytype
generalize env t = Polytype alpha t
    where
        alpha = Set.toList $ freeVariables t `Set.difference` freeVariables env
        
instanciate :: Monad m => Polytype -> Inference m Monotype
instanciate (Polytype as monotype) = do
    env <- Substitutions . Map.fromList <$> mapM mapToNewVar as
    return $ substitute env monotype
    where mapToNewVar t =fmap (\v -> (t, v)) newVariable
          
infer :: Monad m => Environment -> NonTypedExpr -> Inference m (Substitutions, Monotype, TypedExpr)
infer env (Var name)        = do 
    t <- findType (getName name) env
    t' <- instanciate t
    return (mempty, t', Var (t'<$ name))
infer env (Apply expr1 expr2) = do
    (s1, t1, e1') <- infer env expr1
    (s2, t2, e2' ) <- infer (substitute s1 env) expr2
    beta <- newVariable
    s3 <- unify (substitute s2 t1) (createArrow t2 beta)
    return (s1 <> s2 <> s3, substitute s3 beta, Apply e1' e2')

infer env (Lambda x expr) = do
    beta <- newVariable
    let env' = insert (getName x) (toPolytype beta) env 
    (s1, t1, expr') <- infer env' expr
    let paramType = substitute s1 beta
    return (s1, createArrow paramType t1, Lambda (paramType <$ x) expr')
    
infer env (Constant c ()) = 
    let t = constantType c
    in return (mempty, t, (Constant c t))
{-
infer env (Let x expr1 expr2) = do
    (s1, t1) <- infer env expr1
    let env' = substitute s1 env
    (s2, t2) <- infer (insert (getName x) (generalize env' t1) env') expr2
    return (s1 <> s2, t2)
    -}
{-  On suppose que l'ont possède une fonction fix :: (a -> a) -> a, fix f = f (fix f)
    Nous ne pouvons pas encore exprimer cette fonction car elle est récursive, elle est n'est pas encore dans l'environement
    quand nous essayons de déterminer le type
    
    L'idée est que au lieu d'exprimer la récursion comme ça : product n = if (n < 0) then 1 else n * (product (n - 1)),
    nous l'exprimons comme ça: fix (\product n -> if (n < 0) then 1 else n * (product (n - 1))).
    Il suffit simplement de simuler l'application pour avoir le type de la fonction
-}
infer env (Fix x expr) = do 
    a <- newVariable
    let fix  = Arrow (identityType a) a
    (s1, t1, expr') <- infer env expr
    b <- newVariable
    s2 <- unify (substitute s1 fix) (createArrow t1 b)
    let t = substitute s2 b
    return (s1 <> s2, t, Apply expr' expr')