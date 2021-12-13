{-# LANGUAGE BlockArguments #-}

module Main where
  
import Data.Bifunctor (first)
import Dovetail
import Dovetail.Core (core)
import Test.Hspec
  
renderOpts :: RenderValueOptions
renderOpts = RenderValueOptions
  { colorOutput = False
  , maximumDepth = Nothing 
  }
  
main :: IO ()
main = hspec do
  describe "Core Libraries" do            
    it "should build the core libraries without error" do 
      result <- first (renderInterpretError renderOpts) <$> runInterpret () core
      result `shouldBe` Right ()