module TypeInference(inferModule, getType, module T, runInference, TypedExpr)  where
import TypeInference.Base as T
import TypeInference.Rules as T
import TypeInference.TypeVariable as T
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.Functor.Identity
import LambdaCalculus
import qualified Data.Map as Map
import Parser
import Parser.Base
import Parser.Build
import Data.Maybe
import Data.Set
import ModuleSystem



runInference :: Monad m => Inference m a -> ExceptT String m a
runInference e = evalStateT e baseVariable
    
getType :: Monad m => NonTypedExpr -> m (Either String (Expr Monotype))
getType expr = fmap (fmap (\(a,b,c) -> c)) $ runExceptT $ runInference (infer mempty expr)

inferModule :: Monad m => Module [Named Function] -> Inference m (Module [Named (Expr Monotype)])
inferModule = fmap (fmap snd) . mapModuleM inferModuleInner 

type InferInner = (Environment, [Named (Expr Monotype)])

inferModuleInner :: Monad m => InferInner -> [Named Function] -> Inference m InferInner
inferModuleInner env []  = return mempty
inferModuleInner (env, _) ((Named n (Function sign expr)):xs) = do
        (s, t, e) <- infer env expr
        --t' <- specifyUserSignature sign t
        --let expr' = t' <$ e
        (env', xs) <- inferModuleInner (addMonotypeToEnv n t env,[]) xs
        return (env', (Named n e):xs)
        

addMonotypeToEnv :: String -> Monotype -> Environment -> Environment
addMonotypeToEnv name t e@(Environment env) = Environment $ Map.insert name (generalize e t) env

specifyUserSignature :: Monad m => Maybe FunctionSignature -> Monotype -> Inference m Monotype
specifyUserSignature funtionType t = 
    case funtionType >>= getSignature of
         Just t' -> unify t t' >> return t'
         Nothing -> return t

toTVar = TVar . TypeVariable

getSignature :: FunctionSignature -> Maybe Monotype
getSignature [] = Nothing
getSignature xs = Just $ monotype xs
    where 
        monotype [x] = toMono x
        monotype (x:xs)= Arrow (toMono x) (monotype xs)
        toMono (Generic c) = toTVar c
        toMono (Argument c) = TConstant c