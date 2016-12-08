module Parser(parseModule, module P) where

import Text.Megaparsec
import Data.Bifunctor
import Parser.Base as P
import Parser.Build as P
import LambdaCalculus as P
import Data.List
import ModuleSystem
import Control.Monad.Trans.Except

parseModule :: Monad m => String -> String -> ExceptT String m (ParsedModule [(TopLevelName, Function)])
parseModule fileContent fileName = ExceptT $ pure $ do
    parseResult <- first show $ mapM (parse moduleParser fileName) $ separateTopLevel fileContent
    f <- functions <$> createParseOutput parseResult
    return $ ParsedModule fileName [] f
    
fstSpace :: String -> Bool
fstSpace (' ':_) = True
fstSpace _ = False

--go :: [String] -> [[String]]
go [] = []
go (x:xs) = (x ++ concat keep) : go next
    where 
        (keep, next) = span fstSpace xs
separateTopLevel = go . filter (not.null) . lines . fmap (\x -> if x == '\t' then ' ' else x)
    
        