module Parser where

import Text.Megaparsec
import Control.Applicative
import Text.Megaparsec.String

type Constructor = String
data ProductTypeParam = Argument String | Generic String deriving(Show)
data ProductType = ProductType Constructor [ProductTypeParam] deriving(Show)

constructorString :: Parser String
constructorString = liftA2 (:) upperChar $ many alphaNumChar

parameterParser :: Parser ProductTypeParam
parameterParser = do
    p <- Argument <$> constructorString <|> Generic <$> some alphaNumChar 
    space
    return p
    
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
    
