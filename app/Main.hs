module Main where

import Parser
import Closure
import TypeInference
import ModuleSystem
import ExceptionHandling
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))



compile content fileName = do 
    parseResult <- mapError $ parseModule content fileName
    m <- mapError $ createValidGraph [parseResult]
    runInference $ inferModule m
    
    
    
main :: IO ()
main = do
    result <- compile <$> readFile "./example.bae"
    print ()
    
    