-- | Module servant à évaluer un programme
module Interpreter where
import LambdaCalculus
import Closure
import Data.Maybe
import STDLib
import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Map as Map


type Bindings = Map.Map String ExprEnv

-- | Monad reader représentant l'environment global. Un monad reader est le moyen en Haskell de faire une sorte d'injection de dépendance. 
type Evaluation = Reader Bindings

-- |Évalue un programme
evalProgram xs = do
    m <- case Map.lookup "main" b of
        Just x -> return (ExprEval x)
        Nothing-> throwE "No main"
    return $ runReader (eval m) b
    where
        b = createBindings xs
                
-- | Transforme le résultat du module "Closure" en bindings utilisable
createBindings :: [Named (Expr (a, Environment a))] -> Bindings
createBindings =  Map.fromList . fmap (\(Named n x) -> (n, toEvalType x))


-- | Met à jour la valeur d'une closure
updateValue :: String -> ExprEnv -> ExprEnv -> ExprEnv
updateValue s e = fmap (updateEnvValue s (Just $ ExprEval e))

-- | Cherche la valeur d'une variable dans la librairie standard, dans son closure ou bien dans l'environment global
getVariableValue :: Variable (Environment (Maybe ExprEval)) -> Evaluation ExprEnv
getVariableValue (Named n env) = do
    bindings <- ask
    case  Map.lookup n stdLib of
        Just x-> return $ (Constant x mempty)
        Nothing ->
           return $ case join (getEnvValue n env) of
                Just (ExprEval x) -> x
                Nothing -> fromJust $ Map.lookup n bindings
                

                
                
-- | Evalue une expression
eval :: ExprEval -> Evaluation ExprEnv
eval (ExprEval (Apply expr1 expr2)) = applyToLambda expr1 expr2
eval (ExprEval (Var a)) = do 
    var <- ExprEval <$> getVariableValue a
    eval var
eval (ExprEval (If cond a b)) = do
    c' <- eval $ ExprEval cond
    case c' of
         (Constant (LBool True) _) -> eval $ ExprEval a
         _ -> eval $ ExprEval b
eval (ExprEval a) = return a

-- |Applique la beta reduction et les fonctions built in
applyToLambda :: ExprEnv -> ExprEnv -> Evaluation ExprEnv
applyToLambda (Lambda a expr) param = eval $ ExprEval (updateValue (getName a) param expr)
applyToLambda (Fix _ expr) param = applyToLambda expr param
applyToLambda (Constant (BuiltInFunc (Func f)) _) param = do
    (Constant p _) <- eval (ExprEval param)
    return $ Constant (f p) mempty
applyToLambda a param = do 
    expr <- (eval $ ExprEval a)
    applyToLambda expr param