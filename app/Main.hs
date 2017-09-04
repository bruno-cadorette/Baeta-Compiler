module Main where

import Compile
import System.Environment
import Control.Monad.Trans.Writer.Lazy

printResult :: (Show a1, Show a) => Either a a1 -> IO ()
printResult (Left err) = print err
printResult (Right r) = print r
    
getResult f files = do
    (r, logs) <- runWriterT (f files)
    putStrLn logs
    printResult r  
    
errorMessage :: [Char]
errorMessage = "Please use -o to get the output, -t for the type, -p for the parsing output and -c for the closures"
    
handleArgs :: [Char] -> [(String, String)] -> IO ()
handleArgs "-o" = getResult getProgramOutput
handleArgs "-t" = getResult getProgramTypes
handleArgs "-p" = getResult getProgramAST
handleArgs "-c" = getResult getProgramClosures
handleArgs _    = const $ print errorMessage
    
handle :: [FilePath] -> IO ()
handle (x:xs) = do
    contents <- mapM readFile xs
    handleArgs x $ zip xs contents
handle [] = print errorMessage
    
main :: IO ()
main = do
    args <- getArgs 
    handle args