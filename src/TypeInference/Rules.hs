module TypeInference.Rules where

import TypeInference.Base
import Data.Set as Set
import Data.Map as Map
import TypeInference.TypeVariable
import Control.Monad.State
import LambdaCalculus
import Data.Monoid
import Control.Monad.Trans.Except

type Inference a = ExceptT String (State TypeVariable) a

newVariable :: Inference Monotype
newVariable = fmap TVar newTypeVariable

--generalize(Γ, τ ) =def ∀α.τ ~ where α = freevars(τ ) − freevars(Γ)

generalize :: Environment -> Monotype -> Polytype
generalize env t = Polytype alpha t
    where
        alpha = Set.toList $ freeVariables t `Set.difference` freeVariables env
        
instanciate :: Polytype -> Inference Monotype
instanciate (Polytype as monotype) = do
    env <- Substitutions . Map.fromList <$> mapM mapToNewVar as
    return $ substitute env monotype
    where mapToNewVar t =fmap (\v -> (t, v)) newVariable
          
infer :: Environment -> Expr -> Inference (Substitutions, Monotype)
infer env (Var name)        = do 
    t <- findType name env
    t' <- instanciate t
    return (mempty, t')
infer env (Apply expr1 expr2) = do
    (s1, t1) <- infer env expr1
    (s2, t2) <- infer (substitute s1 env) expr2
    beta <- newVariable
    s3 <- unify (substitute s2 t1) (createArrow t2 beta)
    return (s1 <> s2 <> s3, substitute s3 beta)

infer (Environment env) (Lambda x expr) = do
    beta <- newVariable
    let env' = Environment $ Map.insert x (toPolytype beta) env 
    (s1, t1) <- infer env' expr
    return (s1, createArrow (substitute s1 beta) t1)