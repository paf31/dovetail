{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where
  
import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Text (Text)
import Dovetail
import Dovetail.Prelude (prelude)
import GHC.Generics (Generic)
import Language.PureScript.CoreFn qualified as CoreFn
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Text ()
  
main :: IO ()
main = hspec do
  describe "Evaluation" do
    describe "ToValue" do
      let roundtrip 
            :: forall a
             . ( Arbitrary a
               , Show a
               , Eq a
               , ToValue Identity a
               )
            => Property
          roundtrip = property \x -> 
            either (const Nothing) Just 
              (runEval (fromValue (toValue @_ @a x))) === Just x
              
      it "should roundtrip Integer" $ 
        roundtrip @Integer
      it "should roundtrip Double" $ 
        roundtrip @Double
      it "should roundtrip Text" $ 
        roundtrip @Text
      it "should roundtrip records" $ 
        roundtrip @ExampleRecord1
        
      let roundtrip1
            :: forall a b
             . ( CoArbitrary a
               , Arbitrary a, Arbitrary b
               , Show a, Show b
               , Eq a, Eq b
               , ToValue Identity a
               , ToValue Identity b
               )
            => Property
          roundtrip1 = forAllBlind arbitrary \f -> property \a -> 
            either (const Nothing) Just 
              (runEval (fromValueRHS (pure (toValue @_ @(a -> Eval b) (pure . f))) a)) === Just (f a)
              
      it "should roundtrip Text -> Text" $ 
        roundtrip1 @Text @Text
      it "should roundtrip Integer -> Text" $ 
        roundtrip1 @Integer @Text
      it "should roundtrip Text -> Integer" $ 
        roundtrip1 @Integer @Text
      it "should roundtrip Text -> records" $ 
        roundtrip1 @Text @ExampleRecord1
    
  describe "Build" do
    describe "InterpretT" do
      let buildSingleModuleWithPrelude 
            :: forall a
             . ToValueRHS Identity a
            => Text
            -> Either String a
          buildSingleModuleWithPrelude moduleText =
            first renderInterpretError $
              runInterpret do
                ffi prelude
                m <- build moduleText
                evalMain (CoreFn.moduleName m)
              
      -- TODO: replace these with a suite of golden tests:
                
      it "should build single modules from source" $
        fmap (first renderEvaluationError . runEval) 
          (buildSingleModuleWithPrelude @(Eval Integer) 
            "module Main where main = 42")
          `shouldBe` Right (Right 42)
      
      it "should support imports" $
        fmap (first renderEvaluationError . runEval) 
          (buildSingleModuleWithPrelude @(Eval Integer) 
            "module Main where\n\
            \import Prelude\n\
            \main = identity 42")
          `shouldBe` Right (Right 42)
      
      it "should support returning functions" $
        fmap (first renderEvaluationError . runEval . ($ "testing")) 
          (buildSingleModuleWithPrelude @(Text -> Eval Text) 
            "module Main where\n\
            \import Prelude\n\
            \main x = x")
          `shouldBe` Right (Right "testing")
      
      it "should support records" $
        fmap (first renderEvaluationError . runEval . ($ "testing")) 
          (buildSingleModuleWithPrelude @(Text -> Eval ExampleRecord1) 
            "module Main where\n\
            \import Prelude\n\
            \main x = { foo: 42, bar: 1.0, baz: true, quux: x }")
          `shouldBe` Right (Right (ExampleRecord1 42 1.0 True "testing"))
    
data ExampleRecord1 = ExampleRecord1
  { foo :: Integer
  , bar :: Double
  , baz :: Bool
  , quux :: Text
  } deriving stock (Show, Eq, Generic) 
    deriving anyclass (ToValue m)
    deriving Arbitrary via GenericArbitrary ExampleRecord1
