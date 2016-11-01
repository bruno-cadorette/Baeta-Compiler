module Main where

import Parser
import TypeInference
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

t o = case o >>= inferModule of
           Right t -> putDoc $ pretty t
           Left t -> print t
    

main :: IO ()
main = do
    parseOutput <- parseModule <$> readFile "./example.bae"
    t parseOutput
    return ()
    
