module Parser where

import Text.Megaparsec
import Control.Applicative
import Text.Megaparsec.String

type Constructor = String
data ProductTypeParam = Argument String | Generic String deriving(Show)
data ProductType = ProductType Constructor [ProductTypeParam] deriving(Show)

type VariableName = String

data Expr = 
      Var VariableName
    | Apply Expr Expr
    | Lambda VariableName Expr
    -- | Literal Int
    | Function String Expr --This represent a top level function, so "id a = a" will be parsed as "Function "id" (Lambda "a" (Var "a"))". 
                           --Maybe this should be in another datatype, since Function "a" (Function "b" ...) isn't valid
    deriving (Show)

endWithSpace :: Parser a -> Parser a
endWithSpace parser = do
    p <- parser
    space
    return p
  
constructorString :: Parser String
constructorString = liftA2 (:) upperChar $ many alphaNumChar

parameterParser :: Parser ProductTypeParam
parameterParser = endWithSpace $ Argument <$> constructorString <|> Generic <$> some alphaNumChar 

productTypeParser :: Parser ProductType
productTypeParser = do
    constructor <- many alphaNumChar
    space
    params <- many parameterParser
    return $ ProductType constructor params
    
    
--Currently only support product type, so the syntax is "datatype Constructor a b Int" where lowercase are generics and uppercase are type
createTypeParser = do
    string "datatype"
    space
    productTypeParser
    
functionParser :: Parser Expr
functionParser = do
    functionName <- many alphaNumChar
    space
    param <- many alphaNumChar
    space
    char '='
    space
    expr <- expressionParser
    return $ Function functionName (Lambda param expr)
    
--parathesis = between (char "(") (char ")")
lambdaParser :: Parser Expr
lambdaParser = do
    char '\\'
    param <- many alphaNumChar
    space
    string "->"
    space
    expr <- expressionParser
    return $ Lambda param expr
    
expressionParser = lambdaParser <|> varParser -- <|> literal
varParser = Var <$> many alphaNumChar
--literal = digitChar 