module Parser.Build where

import Text.Megaparsec
import Parser.Base
import Control.Applicative
import Text.Megaparsec.String
import Data.Map.Strict as Map (insert, Map, delete, lookup, fromList, toList, keys) 
type TopLevelName = String
type FunctionSignature = [ProductTypeParam]
data Function = Function (Maybe FunctionSignature) Expr deriving(Show) --This represent a top level function, so "id a = a" will be parsed as "Function "id" (Lambda "a" (Var "a"))". 

data ParseOutput = ParseOutput (Map TopLevelName Function) [ProductType] deriving (Show)
    
createModule :: [TempModuleParser] -> (Map TopLevelName Expr, Map String FunctionSignature, [ProductType])
createModule = foldr buildModule mempty
    where
        buildModule (ParseFunction (functionName, expr)) = fst3 (insert functionName expr)
        buildModule (TypeAnnotation (FunctionType functionName params)) = snd3 (insert functionName params)
        buildModule (ParseProductType newType) = trd3 (newType :)

        
mergeHeaderWithFunction :: Map String FunctionSignature -> [(TopLevelName, Expr)] -> Either String (Map TopLevelName Function)
mergeHeaderWithFunction a b = fmap fromList $ mergeHeaderWithFunction' a b
    where 
        mergeHeaderWithFunction' :: Map String FunctionSignature -> [(TopLevelName, Expr)] -> Either String [(String, Function)]
        mergeHeaderWithFunction' typeMap ((functionName, expr):xs) =  
            case Map.lookup functionName typeMap of
                Just t -> ((functionName, Function (Just t) expr) :) <$> mergeHeaderWithFunction' (delete functionName typeMap) xs
                Nothing -> ((functionName, Function Nothing expr) :) <$> mergeHeaderWithFunction' typeMap xs
        mergeHeaderWithFunction' typeMap []
            | null typeMap = Right []
            | otherwise = Left $ "Missing top level function named " ++ show (keys typeMap)
    
createParseOutput :: [TempModuleParser] -> Either String ParseOutput
createParseOutput xs = (\funcs -> ParseOutput funcs types) <$> mergeHeaderWithFunction headers (toList functions)
    where
        (functions, headers, types) = createModule xs
        
fst3 f (a, b, c) = (f a, b, c)
snd3 f (a, b, c) = (a, f b, c)
trd3 f (a, b, c) = (a, b, f c)
        