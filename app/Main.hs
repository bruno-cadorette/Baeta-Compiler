module Main where

import Compile
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment


-- getProgramType str = getType =<< (debugParse str)



printResult (Left err) = print err
printResult (Right r) = print $ pretty r
    
    
main :: IO ()
main = do
    files <- getArgs
    contents <- mapM readFile files
    result <- compile $ zip files contents
    printResult result