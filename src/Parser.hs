module Parser(parseModule) where

import Text.Megaparsec
import Data.Bifunctor
import Parser.Base as P
import Parser.Build as P
import LambdaCalculus as P
import Data.List


parseModule fileContent = do
    parseResult <- first show $ mapM (parse moduleParser "") $ separateTopLevel fileContent
    functions <$> createParseOutput parseResult
    
fstSpace :: String -> Bool
fstSpace (' ':_) = True
fstSpace _ = False

--go :: [String] -> [[String]]
go [] = []
go (x:xs) = (x ++ concat keep) : go next
            where 
                (keep, next) = span fstSpace xs
separateTopLevel = go . filter (not.null) . lines . fmap (\x -> if x == '\t' then ' ' else x)
    
        