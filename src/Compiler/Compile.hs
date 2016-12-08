module Compiler.Compile where
import LambdaCalculus
import TypeInference
import Closure
{-
--simpleFunction = Function Private Nothing Fast
--Expr Monotype
uninit [] = mempty
uninit [x] = ([], Just x)
uninit (x:xs) = first (:) uninit xs

uncurryType (Arrow t1 t2) = uncurryType t1 ++ uncurryType t2
uncurryType (TVar t) = [t]
uncurryType (TConstant t) = [t]

toLLVMType (TConstant "Int") = i32
toLLVMType a@(Arrow t1 t2) = FunctionType (toLLVMType ret) (map toLLVMType args) False
    where 
        (args, Just ret) = first  $ uncurryType a

compileTopLevelFunction :: Named (Expr (Environment Monotype)) -> ()
compileTopLevelFunction (Named name expr) = 
    case expr of
    -}