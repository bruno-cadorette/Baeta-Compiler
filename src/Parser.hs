-- |Module qui utilise Parser.Base et Parser.Build afin de parser un fichier en un module
module Parser(parseModule, module P, parseFunction) where

import Text.Megaparsec
import Data.Bifunctor
import Parser.Base as P
import Parser.Build as P
import LambdaCalculus as P
import ModuleSystem
import Data.List
import Control.Monad.Trans.Except

parseFunction :: Monad m => String -> ExceptT String m (Named Function)
parseFunction = ExceptT . pure . first show . second (\(n, expr) -> Named n $ Function Nothing expr) . parse namedFunctionParser ""

-- |Parse le contenu d'un fichier en un module
parseModule :: Monad m => String -> String -> ExceptT String m (ParsedModule [Named Function])
parseModule fileContent fileName = ExceptT $ pure $ do
    parseResult <- first show $ mapM (parse moduleParser fileName) $ separateTopLevel fileContent
    (ParseOutput f _ i) <- createParseOutput parseResult
    return $ ParsedModule fileName i f
    
fstSpace :: String -> Bool
fstSpace = isPrefixOf " "



spt [] = []
spt (x:xs) = (x ++ concat keep) : spt next
    where 
        (keep, next) = span fstSpace xs
separateTopLevel = spt . filter (not.null) . lines . fmap (\x -> if x == '\t' then ' ' else x)
    
        