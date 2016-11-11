module Main where

import Test.Hspec
import Parser
import TypeInference

main :: IO ()
main = mapM_ hspec [parseTest, typeTest]

parseTest = describe "parse test" $ do
    it "id with type" $ 
        parseModule "id :: a -> a\nid x = x" `shouldBe` Right [("id",Function (Just [Generic "a",Generic "a"]) (Lambda "x" (Var "x")))]
    it "associativity" $
        parseModule "a = f g x"  `shouldBe`  parseModule "a = (f g) x"

typeTest = describe "type test" $ do
    it "id" $ 
        getType (Lambda "x" (Var "x")) `shouldBe` Right (Arrow (TVar (TypeVariable "a")) (TVar (TypeVariable "a")))
    it "apply int" $
        getType (Lambda "x" (Apply (Var "x") (exprInt 7))) `shouldBe` 
            Right (((TConstant "Int") `Arrow` (TVar (TypeVariable "b"))) `Arrow` (TVar (TypeVariable "b")))
 


 