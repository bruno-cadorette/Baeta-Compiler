{-#LANGUAGE ScopedTypeVariables#-}
-- | Librairie standard du langage, contient les fonctions de base d'Haskell pouvant se retrouver dans Baeta
module STDLib where
import LambdaCalculus
import System.IO.Unsafe
import Debug.Trace
import Data.Map
import Closure
import TypeInference.Base
import TypeInference.TypeVariable

func = BuiltInFunc . Func

litToConst a = Constant a mempty 
newNamed a = Named a mempty
mathHelper :: (Integer -> Integer -> Integer) -> Literal
mathHelper op = func $ \(LInt a) -> func $ \(LInt b) -> LInt (op a b)

mathType :: Monad m => Inference m Monotype
mathType = biFunction "Int" "Int" "Int"

biFunction :: Monad m => String -> String -> String -> Inference m Monotype
biFunction a b c = return $ createArrow (TConstant a) (createArrow (TConstant b) (TConstant c))


eqSTD, add, sub, mul, concatStr :: Literal
add = mathHelper (+) 
sub = mathHelper (-) 

concatStr = func $  \(LString a) -> func $ \(LString b) -> (LString $ a ++ b)
mul = mathHelper (*) 


eqSTD = func $ \a -> func $ \b -> (LBool $ a == b )
ltSTD = func $ \(LInt a) -> func $ \(LInt b) -> (LBool $ a < b )
notSTD = func $ \(LBool a) -> LBool (not a)
eqType, ltType, notType :: Monad m => Inference m Monotype
eqType = do
    a <- TVar <$> newTypeVariable
    return $ createArrow a (createArrow a (TConstant "Bool"))
    
ltType = return $ createArrow (TConstant "Int") (createArrow (TConstant "Int") (TConstant "Bool"))
notType = return $ createArrow (TConstant "Bool") (TConstant "Bool")
stdLibBase :: (Monad m) => [(String, Literal, Inference m Monotype)]
stdLibBase = [
        ("add", add, mathType), 
        ("sub", sub, mathType), 
        ("mul", mul, mathType), 
        ("concatStr", concatStr, (biFunction "String" "String" "String")),
        ("eq", eqSTD, eqType),
        ("lt", ltSTD, ltType),
        ("not", notSTD, notType)
    ]
 
-- | Map un nom d'une fonction à son type
stdLibType :: Monad m => Inference m (Map String Monotype)
stdLibType = sequence $ fromList $ fmap (\(n, b, t) -> (n, t)) stdLibBase

-- | Map un nom d'une fonction à sa valeur
stdLib :: Map String Literal
stdLib = fromList $ fmap (\(n, b, t::Inference IO Monotype) -> (n, b)) stdLibBase