module Parser.Base where
    
import Text.Megaparsec
import Control.Applicative
import LambdaCalculus
import Text.Megaparsec.String

type Constructor = String
data ProductTypeParam = Argument String | Generic String deriving(Show, Eq)
data ProductType = ProductType Constructor [ProductTypeParam] deriving(Show, Eq)


data FunctionType = FunctionType String [ProductTypeParam] deriving(Show) 

data TempModuleParser = ParseFunction (String, Expr) | TypeAnnotation FunctionType | ParseProductType ProductType deriving (Show)

endWithSpace :: Parser a -> Parser a
endWithSpace parser = do
    p <- parser
    space
    return p
  
constructorString :: Parser String
constructorString = liftA2 (:) upperChar $ many alphaNumChar

functionNameString :: Parser String
functionNameString = liftA2 (:) lowerChar $ many alphaNumChar

parameterParser :: Parser ProductTypeParam
parameterParser = Argument <$> constructorString <|> Generic <$> some alphaNumChar 

productTypeParser :: Parser ProductType
productTypeParser = do
    constructor <- constructorString
    space
    params <- many (endWithSpace parameterParser)
    return $ ProductType constructor params
    
    
--Currently only support product type, so the syntax is "datatype Constructor a b Int" where lowercase are generics and uppercase are type
createTypeParser = do
    string "datatype"
    space
    productTypeParser
    
functionParser :: String -> Parser (String, Expr)
functionParser functionName = do
    param <- many $ endWithSpace $ some alphaNumChar
    char '='
    space
    expr <- expressionParser
    return $ (functionName, (currying param expr))

currying :: [String] -> Expr ->  Expr
currying (x:xs) expr = Lambda x (currying xs expr)
currying []     expr = expr
    
lambdaParser :: Parser Expr
lambdaParser = do
    char '\\'
    param <- some $ endWithSpace $ some alphaNumChar
    space
    string "->"
    space
    expr <- expressionParser
    return $ currying param expr

applyParser :: Expr -> Parser Expr
applyParser expr = do
    space
    v <- optional expressionParser
    case v of
        Just x -> applyParser $ Apply expr x 
        Nothing -> return expr

expressionParser :: Parser Expr        
expressionParser = do 
    p <- (between (char '(') (char ')') expressionParser) <|> lambdaParser <|> varParser
    applyParser p
    
varParser = Var <$> some alphaNumChar

signatureParser :: String -> Parser FunctionType
signatureParser functionName = do
    string "::"
    space
    params <- sepBy1 parameterParser (space >> string "->" >> space)
    return $ FunctionType functionName params
        
        
functionOrSignatureParser :: Parser TempModuleParser
functionOrSignatureParser = do
    name <- functionNameString
    space
    (TypeAnnotation <$> signatureParser name) <|> (ParseFunction <$> functionParser name)
    
moduleParser :: Parser TempModuleParser
moduleParser =
    (ParseProductType <$> createTypeParser) <|> 
    functionOrSignatureParser