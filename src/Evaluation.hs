module Evaluation where
import LambdaCalculus
import qualified Data.Map as Map


toEither _ (Just a) = Right a
toEither a _ = Left a

type Environment = Map.Map String Value
type Evaluate a = Either String a

getInt :: Value -> Evaluate Integer
getInt (VConstant (LInt i)) = return i
getInt a = Left ("expected Integer but got " ++ show a)

data Value = 
      VConstant Literal
    | Closure Expr Environment
    deriving (Show)
    {-fixPoint = -} 
    
eval :: Environment -> Expr -> Evaluate Value
eval env (Var v) = toEither ("Can't find variable " ++ v ++ " in the typing environment") $ Map.lookup v env
eval env l@(Lambda x expr) = return $ Closure l env
eval env (Apply e1 e2) = 
    
            
applyToLambda env (Apply (Lambda name expr1) expr2) = do
    v <- eval env expr2
    eval (Map.insert name v env) expr1
op :: BuiltInFunction -> Integer -> Integer -> Integer
op Add = (+)
op Sub = (-)
op Mul = (*)
op Div = div

evalBuiltIn :: BuiltInFunction -> Environment -> Expr -> Expr -> Evaluate Value
evalBuiltIn f env expr1 expr2 = do
    a <- getInt =<< eval env expr1
    b <- getInt =<< eval env expr2
    return $ VConstant $ LInt $ (op f) a b
    
    
    
    
f1 = Lambda "x" (Lambda "y" (Apply (Var "y") (Var "x")))
f2 = Apply (Apply (BuiltInFunc Add) (Constant (LInt 2))) (Constant (LInt 2))
f3 = Lambda "x" (Apply (Apply (BuiltInFunc Add) (Var "x")) (Constant (LInt 1)))
    
-- (\x. \y. y x) (2 + 2) (\x. x + 1)

example =  Apply (Apply f1 f2) f3