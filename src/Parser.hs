module Parser where

import Text.Megaparsec
import Data.Bifunctor
import Parser.Base
import Parser.Build

parseModule fileContent = do
    parseResult <- first show $ parse moduleParser "" fileContent
    createParseOutput parseResult