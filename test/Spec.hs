{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Parser
import ModuleSystem
import Compile
import TypeInference
import Control.Monad.Trans.Except
import Data.Functor.Identity

main :: IO ()
main = do
    mapM_ hspec [parseTest, typeTest]
    quickCheck $ forAll genPositive propFactorial

parseTest = describe "parse test" $ do
    it "id with type" $ 
        moduleValue <$> runExcept (parseModule "id :: a -> a\nid x = x" "")  `shouldBe` Right [(Named "id" $ Function (Just [Generic "a",Generic "a"]) (Lambda (Named "x" ()) (Var $ Named "x" ())))]
    it "associativity" $
        runExcept (parseModule "a = f g x" "")  `shouldBe`  runExcept (parseModule "a = (f g) x" "")

typeTest = describe "type test" $ do
    it "id" $ 
        getType (Lambda (Named "x" ()) (Var (Named "x" ()))) `shouldBe` 
            Right (Arrow (TVar (TypeVariable "a")) (TVar (TypeVariable "a")))
    it "apply int" $
        getType (Lambda (Named "x" ()) (Apply (Var (Named "x" ())) (exprInt 7 ()))) `shouldBe` 
        Right (((TConstant "Int") `Arrow` (TVar (TypeVariable "b"))) `Arrow` (TVar (TypeVariable "b")))
 

genPositive = abs <$> (arbitrary :: Gen Integer) `suchThat` (\i -> i > 0)

propFactorial n = 
    runIdentity (compile [("", facBaeta n)]) == Right (toFacResult (fac n))
    where 
        toFacResult x = Constant (LInt x) mempty
 
facBaeta n = "f n = #if (eq n 0) 1 (mul n (f (sub n 1)))\n\nmain = f " ++ show n

fac 0 = 1
fac n = n * (fac (n - 1))
