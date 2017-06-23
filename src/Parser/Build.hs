-- |Ce module analyse le résultat du parsing et le transforme en structure de donné plus facile a utilisé

module Parser.Build where

import Parser.Base
import Data.Maybe
import LambdaCalculus
import Data.Map.Strict as Map (insert, Map, delete, lookup, keys) 
type TopLevelName = String
type FunctionSignature = [ProductTypeParam]
-- |Représente une fonction avec sa signature 
data Function = Function (Maybe FunctionSignature) NonTypedExpr deriving(Show, Eq) 
-- |Structure de donnée pour travailler avec le résultat du parsing
data ParseOutput = ParseOutput { functions :: [Named Function], types :: [ProductType], imports :: [String]} deriving (Show)
    
createModule :: [TempModuleParser] -> ([(TopLevelName, NonTypedExpr)], Map String FunctionSignature, [ProductType])
createModule = foldr buildModule mempty
    where
        buildModule (ParseFunction (functionName, expr)) = fst3 ((functionName, expr) :)
        buildModule (TypeAnnotation (FunctionType functionName params)) = snd3 (insert functionName params)
        buildModule (ParseProductType newType) = trd3 (newType :)
        buildModule _ = id

-- |Combine les entêtes de fonction avec la fonction elle même
mergeHeaderWithFunction :: Map String FunctionSignature -> [(TopLevelName, NonTypedExpr)] -> Either String [(Named Function)]
mergeHeaderWithFunction typeMap ((functionName, expr):xs) =  
    case Map.lookup functionName typeMap of
        Just t -> ((Named functionName $ Function (Just t) expr) :) <$> mergeHeaderWithFunction (delete functionName typeMap) xs
        Nothing -> ((Named functionName $ Function Nothing expr) :) <$> mergeHeaderWithFunction typeMap xs
mergeHeaderWithFunction typeMap []
    | null typeMap = Right []
    | otherwise = Left $ "Missing top level function named " ++ show (keys typeMap)
    
-- |Map une liste de TempModuleParser à un ParseOutput, ou une erreur
createParseOutput :: [TempModuleParser] -> Either String ParseOutput
createParseOutput xs = (\funcs -> ParseOutput funcs types imp) <$> mergeHeaderWithFunction headers (fmap (\(n, e) -> (n, transformToFixPoint n e)) functions)
    where
        imp = mapMaybe getImport xs 
        (functions, headers, types) = createModule xs
        
fst3 f (a, b, c) = (f a, b, c)
snd3 f (a, b, c) = (a, f b, c)
trd3 f (a, b, c) = (a, b, f c)

getImport (Import str) = Just str
getImport _ = Nothing        