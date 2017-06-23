
module Main where

import Compile
import TypeInference
import ExceptionHandling
import Parser.Base
import Data.Bifunctor
import Control.Monad.Trans.Except
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment

printResult (Left err) = print err
printResult (Right r) = print r
    
getResult f files = do
    printResult (f files)  
    
errorMessage = "Please use -o to get the output, -t for the type, -p for the parsing output and -c for the closures"
    
handleArgs "-o" = getResult getProgramOutput
handleArgs "-t" = getResult getProgramTypes
handleArgs "-p" = getResult getProgramAST
handleArgs "-c" = getResult getProgramClosures
handleArgs _    = const $ print errorMessage
    
handle (x:xs) = do
    contents <- mapM readFile xs
    handleArgs x $ zip xs contents
handle [] = print errorMessage
    
main :: IO ()
main = do
    args <- getArgs
    handle args