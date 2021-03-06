-- | Module servant a faire l'inférence de type à l'aide des règles de Hindley Milner
module TypeInference.Rules where

import TypeInference.Base
import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeInference.TypeVariable
import LambdaCalculus
import Data.Monoid

type TypedExpr = Expr Monotype

newVariable :: Monad m => InferenceMonad m Monotype
newVariable = fmap TVar newTypeVariable

constantType :: Literal -> Monotype
constantType Unit = TConstant "()"
constantType (LInt _) = TConstant "Int"
constantType (LString _) = TConstant "String"
constantType (LBool _) = TConstant "Bool"

identityType :: Monotype -> Monotype
identityType t = createArrow t t

insert :: String -> Polytype -> TypeEnvironment -> TypeEnvironment
insert key val (TypeEnvironment env) = TypeEnvironment $ Map.insert key val env 

-- Ajoute des variables libres au monotype
-- |generalize(Γ, τ ) =def ∀α.τ ~ where α = freevars(τ ) − freevars(Γ)
generalize :: TypeEnvironment -> Monotype -> Polytype
generalize env t = Polytype alpha t
    where
        alpha = Set.toList $ freeVariables t `Set.difference` freeVariables env
        
-- | Transforme un type polymorphique en un type normal à l'aide des subtitutions
instanciate :: Monad m => Polytype -> InferenceMonad m Monotype
instanciate (Polytype as monotype) = do
    env <- Substitutions . Map.fromList <$> mapM mapToNewVar as
    return $ substitute env monotype
    where mapToNewVar t =fmap (\v -> (t, v)) newVariable
          
{-|  
    Infere le type d'une expression
    
    On suppose que l'ont possède une fonction @fix :: (a -> a) -> a, fix f = f (fix f)@
    Nous ne pouvons pas encore exprimer cette fonction car elle est récursive, elle est n'est pas encore dans l'environement
    quand nous essayons de déterminer le type
    
    L'idée est que au lieu d'exprimer la récursion comme ça : @product n = if (n < 0) then 1 else n * (product (n - 1))@,
    nous l'exprimons comme ça: @fix (\product n -> if (n < 0) then 1 else n * (product (n - 1)))@.
    Il suffit simplement de simuler l'application pour avoir le type de la fonction
-}

 

inferLambdaExpr ::
  Monad m =>
  TypeEnvironment
  -> LambdaExpr Expr ()
  -> InferenceMonad m (Substitutions, Monotype, Expr Monotype)
inferLambdaExpr env (Var name) = do 
    t <- findType (getName name) env
    t' <- instanciate t
    return (mempty, t', LC $ Var (t'<$ name))
inferLambdaExpr env (Apply expr1 expr2) = do
    (s1, t1, e1') <- infer env expr1
    (s2, t2, e2' ) <- infer (substitute s1 env) expr2
    beta <- newVariable
    s3 <- unify (substitute s2 t1) (createArrow t2 beta)
    return (s1 <> s2 <> s3, substitute s3 beta, LC $ Apply e1' e2')

inferLambdaExpr env (Lambda x expr) = do
    beta <- newVariable
    let env' = insert (getName x) (toPolytype beta) env 
    (s1, t1, expr') <- infer env' expr
    let paramType = substitute s1 beta
    return (s1, createArrow paramType t1, LC $ Lambda (paramType <$ x) expr')

infer :: Monad m => TypeEnvironment -> NonTypedExpr -> InferenceMonad m (Substitutions, Monotype, TypedExpr)
infer env (LC a) = inferLambdaExpr env a
infer _ (Constant c ()) = 
    let t = constantType c
    in return (mempty, t, (Constant c t))
infer env (Fix n expr) = do 
    a <- newVariable
    let fix = Arrow (identityType a) a
    (s1, t1, LC (Lambda _ expr')) <- infer env $ LC $ Lambda (Named n ()) expr
    b <- newVariable
    s2 <- unify (substitute s1 fix) (createArrow t1 b)
    return (s1 <> s2, substitute s2 b, Fix n expr')
infer env (If cond a b) = do
    (s1, t1, c') <- infer env cond
    (s2, t2, a') <- infer env a
    (s3, t3, b') <- infer env b
    s4 <- unify t1 $ TConstant "Bool"
    s5 <- unify t2 t3
    return (s1<>s2<>s3<>s4<>s5, substitute s5 t2, If c' a' b')