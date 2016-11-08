module Main where

import Parser
import TypeInference
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
{-
t o = case o >>= inferModule mempty of
           Right t -> putDoc $ pretty t
           Left t -> print t
    
getEnvironment = t . parseModule
    
main :: IO ()
main = do
    parseOutput <- readFile "./example.bae"
    getEnvironment parseOutput
    return ()
    
    -}
    
main = return ()