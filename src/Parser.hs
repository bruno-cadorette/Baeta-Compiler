module Parser where

import Text.Megaparsec
import Control.Applicative
import Text.Megaparsec.String

type Constructor = String
data ProductTypeParam = Argument String | Generic String deriving(Show)
data ProductType = ProductType Constructor [ProductTypeParam] deriving(Show)

type VariableName = String
data Function = Function String Expr deriving(Show) --This represent a top level function, so "id a = a" will be parsed as "Function "id" (Lambda "a" (Var "a"))". 

data Expr = 
      Var VariableName
    | Apply Expr Expr
    | Lambda VariableName Expr
    -- | Literal Int
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
    constructor <- some alphaNumChar
    space
    params <- some parameterParser
    return $ ProductType constructor params
    
    
--Currently only support product type, so the syntax is "datatype Constructor a b Int" where lowercase are generics and uppercase are type
createTypeParser = do
    string "datatype"
    space
    productTypeParser
    
functionParser :: Parser Function
functionParser = do
    functionName <- some alphaNumChar
    space
    param <- some alphaNumChar
    space
    char '='
    space
    expr <- expressionParser
    return $ Function functionName (Lambda param expr)

lambdaParser :: Parser Expr
lambdaParser = do
    char '\\'
    param <- many alphaNumChar
    space
    string "->"
    space
    expr <- expressionParser
    return $ Lambda param expr

applyParser :: Expr -> Parser Expr
applyParser expr = do
    space
    v <- optional expressionParser
    case v of
        Just x -> applyParser $ Apply expr x 
        Nothing -> return expr

expressionParser :: Parser Expr        
expressionParser = do 
    p <- lambdaParser <|> varParser
    applyParser p
    
varParser = Var <$> some alphaNumChar