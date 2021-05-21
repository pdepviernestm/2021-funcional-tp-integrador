module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
   describe "TP Integrador" $ do
     it "2 + 2 es 4" $ do
       2 + 2 `shouldBe` 4

escribime :: Expectation
escribime = implementame

