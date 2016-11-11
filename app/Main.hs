module Main where

import Parser
import TypeInference
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

compile str = do 
    parseResult <- parseModule str
    runInference $ inferModule mempty parseResult
    
    
main :: IO ()
main = do
    result <- compile <$> readFile "./example.bae"
    print result