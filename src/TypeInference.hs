-- | Module qui s'occupe de faire l'inférence des types d'un module 
module TypeInference(inferModule, getType, module T, runInferenceT, inferModuleTypeEnv, inferFunction)  where
import TypeInference.Base as T
import TypeInference.Rules as T
import TypeInference.TypeVariable as T
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Functor.Identity
import LambdaCalculus
import qualified Data.Map as Map
import Parser
import ModuleSystem
import STDLib
import Data.Monoid

runInferenceT :: Monad m => InferenceMonad m a -> m (Either String a, String)
runInferenceT e= runWriterT $ runExceptT $ evalStateT e baseVariable

-- | Fonction de test pour avoir le type d'un expression
getType :: NonTypedExpr -> Either String Monotype
getType expr = runIdentity $ fmap (fmap (\(a,b,c) -> b). fst) $ runInferenceT (infer mempty expr)

-- | Infere les types d'un module
inferModule :: Monad m => Module [Named Function] -> InferenceMonad m (Module [Named (Expr Monotype)])
inferModule = fmap (fmap snd) . mapModuleM (\(env, _) xs -> inferModuleInner env xs)

inferModuleTypeEnv :: Monad m => Module [Named Function] -> InferenceMonad m (Module TypeEnvironment)
inferModuleTypeEnv = fmap (fmap fst) . mapModuleM (\(env, _) xs -> inferModuleInner env xs)

inferFunction :: Monad m => TypeEnvironment -> Named Function -> InferenceMonad m (TypeEnvironment, Named TypedExpr)
inferFunction env (Named n (Function sign expr)) = do
    lib <- TypeEnvironment . Map.map (generalize env) <$> stdLibType
    (_, t, e) <- infer (env <> lib) expr
    t' <- specifyUserSignature sign t
    return (addMonotypeToEnv n t' env, Named n e)


inferModuleInner :: Monad m => TypeEnvironment -> [Named Function] -> InferenceMonad m (TypeEnvironment, [Named TypedExpr])
inferModuleInner _ []  = return mempty
inferModuleInner env (x:xs) = do
    (env', x') <- inferFunction env x
    (env'', xs') <- inferModuleInner env' xs
    return (env'', x': xs')

addMonotypeToEnv :: String -> Monotype -> TypeEnvironment -> TypeEnvironment
addMonotypeToEnv name t e@(TypeEnvironment env) = TypeEnvironment $ Map.insert name (generalize e t) env

specifyUserSignature :: Monad m => Maybe FunctionSignature -> Monotype -> InferenceMonad m Monotype
specifyUserSignature funtionType t = 
    case funtionType >>= getSignature of
         Just t' -> unify t t' >> return t'
         Nothing -> return t

toTVar :: Int -> Monotype
toTVar = TVar . TypeVariable

getSignature :: FunctionSignature -> Maybe Monotype
getSignature [] = Nothing
getSignature xs = Just $ monotype xs
    where 
        monotype [x] = toMono x
        monotype (x:xs)= Arrow (toMono x) (monotype xs)
        toMono (Generic c) = toTVar 0
        toMono (Argument c) = TConstant c