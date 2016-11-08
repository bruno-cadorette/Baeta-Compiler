module Main where

import Test.Hspec
import Parser

main :: IO ()
main = hspec parseId

parseId = describe "id function" $ 
    it "with type" $ 
    parseModule "id :: a -> a\nid x = x" `shouldBe` Right [("id",Function (Just [Generic "a",Generic "a"]) (Lambda "x" (Var "x")))]


