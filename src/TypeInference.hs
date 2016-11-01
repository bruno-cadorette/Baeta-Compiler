module TypeInference(inferModule)  where
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

runInference e =  runIdentity $ evalStateT (runExceptT e ) baseVariable
    
identityFunc = Lambda "x" (Var "y")
inferModule :: [(String, Function)] -> Either String Environment
inferModule xs = runInference $ inferModuleInner xs emptyEnv
        
inferModuleInner :: [(String, Function)] -> Environment -> Inference Environment
inferModuleInner [] env = return env
inferModuleInner ((name, (Function sign expr)):xs) env = do
        (s, t) <- infer env expr
        t' <- specifyUserSignature sign t
        inferModuleInner xs (addMonotypeToEnv name t' env)

addMonotypeToEnv :: String -> Monotype -> Environment -> Environment
addMonotypeToEnv name t e@(Environment env) = Environment $ Map.insert name (generalize e t) env

specifyUserSignature :: Maybe FunctionSignature -> Monotype -> Inference Monotype
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