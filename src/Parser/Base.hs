{-|
Ce module contient tout les combinateurs pour le parsing. La librairie Megaparsec utilise des combinateurs afin de construire d'autres
combinateurs. Par exemple, si on veut parser une string suivi d'un espace, suivi de nomble, la fonction serait quelque chose comme celle ci
@
parser = some lowerChar >>= \l -> space >>= _ -> integer >>= \i -> return (l, i)
@
où lowerChar, space et integer sont des simple combinateurs, some est un combinateur qui prend un autre combinateur en paramètre afin de |l'appliquer 1 ou plusieurs fois. L'opérateur ">>=" est le bind du Monad, utilisé pour combiner les différents parseur dans l'ordre demandé.

Ce module contient presque seulement des combinateurs, le parsing en tant que tel ne se fait pas ici

-}
module Parser.Base where

import Text.Megaparsec
import Control.Applicative
import LambdaCalculus
import Text.Megaparsec.Lexer(integer)
import Text.Megaparsec.String

type Constructor = String
-- |Déclaration de type, pas vraiment utilisé
data ProductTypeParam = Argument String | Generic String deriving(Show, Eq)
-- |Déclaration de type, pas vraiment utilisé
data ProductType = ProductType Constructor [ProductTypeParam] deriving(Show, Eq)

-- |Le type d'une fonction
data FunctionType = FunctionType String [ProductTypeParam] deriving(Show)

-- |Résultat des combinateurs
data TempModuleParser = ParseFunction (String, NonTypedExpr) | TypeAnnotation FunctionType | ParseProductType ProductType | Import String deriving (Show)

-- |Helper
endWithSpace :: Parser a -> Parser a
endWithSpace parser = do
    p <- parser
    space
    return p

-- |Combinateur pour la déclaration des types
constructorString :: Parser String
constructorString = liftA2 (:) upperChar $ many alphaNumChar

-- |Helper
startWithLowerCase :: Parser String
startWithLowerCase = liftA2 (:) lowerChar $ many alphaNumChar

-- |Combinateur pour la déclaration des types
parameterParser :: Parser ProductTypeParam
parameterParser = Argument <$> constructorString <|> Generic <$> some alphaNumChar

-- |Combinateur pour la déclaration des types
productTypeParser :: Parser ProductType
productTypeParser = do
    constructor <- constructorString
    space
    params <- many (endWithSpace parameterParser)
    return $ ProductType constructor params

-- |Combinateur pour la déclaration des types
createTypeParser = do
    string "datatype"
    space
    productTypeParser

-- |Combinateur pour les fonctions
functionParser :: String -> Parser (String, NonTypedExpr)
functionParser functionName = do
    param <- many $ endWithSpace $ some alphaNumChar
    char '='
    space
    expr <- expressionParser
    return (functionName, currying param expr)

namedFunctionParser :: Parser (String, NonTypedExpr)
namedFunctionParser = do
    name <- startWithLowerCase
    space
    functionParser name

-- |Prend la liste des arguments de la fonction et les transformes en argument de lambda
-- |currying ["a","b","c"] (Var "a") == λ a . λ b . λ c . a
currying :: [String] -> NonTypedExpr ->  NonTypedExpr
currying xs expr = foldr (\x -> LC . Lambda (nonTypedVar x)) expr xs

-- |Combinateur pour les lambdas "\[parametres] -> expr"
lambdaParser :: Parser NonTypedExpr
lambdaParser = do
    char '\\'
    param <- some (endWithSpace startWithLowerCase)
    space
    string "->"
    space
    expr <- expressionParser
    return $ currying param expr


-- |Combinateur pour les litéraux de string
strParser :: Parser Literal
strParser = LString <$> between (char '\"') (char '\"') (some (noneOf "\""))

-- |Combinateur pour les litéraux "string | int"
literalParser :: Parser NonTypedExpr
literalParser = fmap (\l -> Constant l ()) (LInt <$> integer <|> strParser)

-- |Combinateur pour le #if
ifParser = do
    string "#if"
    space
    a <- expParser
    space
    b <- expParser
    space
    c <- expParser
    return $ If a b c

-- |Combinateur pour s'assurer que les applications de fonction se fassent dans un bon ordre, c'est à dire que "f g x == (f g) x"
expressionParser :: Parser NonTypedExpr
expressionParser = do
    (x:xs) <- sepBy1 expParser space
    return $ foldl (\x y -> LC $ Apply x y) x xs


-- |Combinateur d'une fonction
expParser = between (char '(') (char ')') expressionParser <|> lambdaParser <|> varParser <|> literalParser <|> ifParser

-- |Combinateur d'une variable
varParser = LC . Var . nonTypedVar <$> startWithLowerCase

-- |Combinateur d'une signature de fonction "fonction :: type"
signatureParser :: String -> Parser FunctionType
signatureParser functionName = do
    string "::"
    space
    params <- sepBy1 parameterParser (space >> string "->" >> space)
    return $ FunctionType functionName params

debugParse = parse expParser ""

-- |Combinateur de l'import
importParse = do
    char '@'
    Import <$> some asciiChar

-- |Combinateur d'une fonction u bien d'une signature
functionOrSignatureParser :: Parser TempModuleParser
functionOrSignatureParser = do
    name <- startWithLowerCase
    space
    (TypeAnnotation <$> signatureParser name) <|> (ParseFunction <$> functionParser name)

-- |Point d'entré du parsing d'un fichier, est représenté par "type | fonction | import"
moduleParser :: Parser TempModuleParser
moduleParser =
    (ParseProductType <$> createTypeParser) <|>
    functionOrSignatureParser <|>
    importParse
