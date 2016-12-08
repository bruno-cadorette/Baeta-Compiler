module Parser.Build where

import Text.Megaparsec
import Parser.Base
import Control.Applicative
import LambdaCalculus
import Text.Megaparsec.String
import Data.Map.Strict as Map (insert, Map, delete, lookup, fromList, toList, keys) 
type TopLevelName = String
type FunctionSignature = [ProductTypeParam]
data Function = Function (Maybe FunctionSignature) NonTypedExpr deriving(Show, Eq) --This represent a top level function, so "id a = a" will be parsed as "Function "id" (Lambda "a" (Var "a"))". 

data ParseOutput = ParseOutput { functions :: [(TopLevelName, Function)], types :: [ProductType]} deriving (Show)
    
createModule :: [TempModuleParser] -> ([(TopLevelName, NonTypedExpr)], Map String FunctionSignature, [ProductType])
createModule = foldr buildModule mempty
    where
        buildModule (ParseFunction (functionName, expr)) = fst3 ((functionName, expr) :)
        buildModule (TypeAnnotation (FunctionType functionName params)) = snd3 (insert functionName params)
        buildModule (ParseProductType newType) = trd3 (newType :)


mergeHeaderWithFunction :: Map String FunctionSignature -> [(TopLevelName, NonTypedExpr)] -> Either String [(String, Function)]
mergeHeaderWithFunction typeMap ((functionName, expr):xs) =  
    case Map.lookup functionName typeMap of
        Just t -> ((functionName, Function (Just t) expr) :) <$> mergeHeaderWithFunction (delete functionName typeMap) xs
        Nothing -> ((functionName, Function Nothing expr) :) <$> mergeHeaderWithFunction typeMap xs
mergeHeaderWithFunction typeMap []
    | null typeMap = Right []
    | otherwise = Left $ "Missing top level function named " ++ show (keys typeMap)
    
createParseOutput :: [TempModuleParser] -> Either String ParseOutput
createParseOutput xs = (\funcs -> ParseOutput funcs types) <$> mergeHeaderWithFunction headers (fmap (\(n, e) -> (n, transformToFixPoint n e)) functions)
    where
        (functions, headers, types) = createModule xs
        
fst3 f (a, b, c) = (f a, b, c)
snd3 f (a, b, c) = (a, f b, c)
trd3 f (a, b, c) = (a, b, f c)
        