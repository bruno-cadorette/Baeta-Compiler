-- | Module qui s'occupe de faire l'inférence des types d'un module 
module TypeInference(inferModule, getType, module T, runInference)  where
import TypeInference.Base as T
import TypeInference.Rules as T
import TypeInference.TypeVariable as T
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.Functor.Identity
import LambdaCalculus
import qualified Data.Map as Map
import Parser
import ModuleSystem
import STDLib
import Data.Monoid

runInference :: Monad m => Inference m a -> ExceptT String m a
runInference e = evalStateT e baseVariable

-- | Fonction de test pour avoir le type d'un expression
getType :: NonTypedExpr -> Either String Monotype
getType expr = runIdentity $ fmap (fmap (\(a,b,c) -> b)) $ runExceptT $ runInference (infer mempty expr)

-- | Infere les types d'un module
inferModule :: Monad m => Module [Named Function] -> Inference m (Module [Named (Expr Monotype)])
inferModule = fmap (fmap snd) . mapModuleM inferModuleInner 

type InferInner = (TypeEnvironment, [Named (Expr Monotype)])

inferModuleInner :: Monad m => InferInner -> [Named Function] -> Inference m InferInner
inferModuleInner env []  = return mempty
inferModuleInner (env, _) ((Named n (Function sign expr)):xs) = do
        lib <- TypeEnvironment . Map.map (generalize env) <$> stdLibType
        (s, t, e) <- infer (env <> lib) expr
        t' <- specifyUserSignature sign t
        (env', xs) <- inferModuleInner (addMonotypeToEnv n t' env,[]) xs
        return (env', (Named n e):xs)
        

addMonotypeToEnv :: String -> Monotype -> TypeEnvironment -> TypeEnvironment
addMonotypeToEnv name t e@(TypeEnvironment env) = TypeEnvironment $ Map.insert name (generalize e t) env

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