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
{-# LANGUAGE TupleSections       #-}

module Main where
  
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as IO
import Data.Vector qualified as Vector
import Dovetail
import Dovetail.Prelude (prelude, stdlib)
import GHC.Generics (Generic)
import Language.PureScript qualified as P
import Language.PureScript.CoreFn qualified as CoreFn
import System.FilePath (takeFileName, (</>))
import System.Directory (listDirectory)
import Test.Hspec
import Test.Hspec.Golden
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Text ()
  
renderOpts :: RenderValueOptions
renderOpts = RenderValueOptions
  { colorOutput = False
  , maximumDepth = Nothing 
  }
  
main :: IO ()
main = hspec do
  describe "Evaluation" do
    describe "ToValue" do
      let roundtrip 
            :: forall a
             . ( Arbitrary a
               , Show a
               , Eq a
               , ToValue () a
               )
            => Property
          roundtrip = property \x -> ioProperty do
            result <- runEval () (fromValue (toValue @_ @a x))
            pure (either (const Nothing) Just result === Just x)
              
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
               , ToValue () a
               , ToValue () b
               )
            => Property
          roundtrip1 = forAllBlind arbitrary \f -> property \a -> ioProperty do
            a' <- runEval () (fromValueRHS (pure (toValue @_ @(a -> Eval () b) (pure . f))) a)
            pure (either (const Nothing) Just a' === Just (f a))
              
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
            :: forall a b
             . (ToValueRHS () a, ToValue () b)
            => Text
            -> (a -> Eval () b)
            -> IO (Either String b)
          buildSingleModuleWithPrelude moduleText f =
            first (renderInterpretError renderOpts) <$>
              runInterpret () do
                ffi prelude
                m <- build moduleText
                a <- evalMain (CoreFn.moduleName m)
                liftEval (f a)
                
      it "should build single modules from source" do
        result <- buildSingleModuleWithPrelude @_ @Integer
          "module Main where main = 42"
          id
        result `shouldBe` Right 42
      
      it "should support imports" do
        result <- buildSingleModuleWithPrelude @_ @Integer
          "module Main where\n\
          \import Prelude\n\
          \main = identity 42"
          id
        result `shouldBe` Right 42
      
      it "should support returning functions" do
        result <- buildSingleModuleWithPrelude @(Text -> Eval () Text) @Text
          "module Main where\n\
          \import Prelude\n\
          \main x = x"
          (\f -> f "testing")
        result `shouldBe` Right "testing"
      
      it "should support records" do
        result <- buildSingleModuleWithPrelude @(Text -> Eval () ExampleRecord1) @ExampleRecord1
          "module Main where\n\
          \import Prelude\n\
          \main x = { foo: 42, bar: 1.0, baz: true, quux: x }"
          (\f -> f "testing")
        result `shouldBe` Right (ExampleRecord1 42 1.0 True "testing")

      it "should support mixing module and expression evaluation" do
        x <- runInterpret () do
               traverse_ ffi stdlib
               let moduleText =
                     "module Main where\n\
                     \import Prelude.Array\n\
                     \main = map _.foo"
               CoreFn.Module { CoreFn.moduleName = mn } <- build moduleText
               (a, _) <- eval (Just mn) "main [{ foo: 42 }]"
               liftEval a
        first (renderInterpretError renderOpts) x
          `shouldBe` Right (Vector.fromList [42 :: Integer])
          
      let buildSingleExpressionWithPrelude 
            :: forall a
             . ToValue () a
            => Bool
            -> [FFI ()]
            -> Text
            -> IO (Either String (a, P.SourceType))
          buildSingleExpressionWithPrelude importPreludeUnqualified ffiModules exprText =
            first (renderInterpretError renderOpts) <$>
              runInterpret () do
                traverse_ ffi ffiModules
                let defaultModule = guard importPreludeUnqualified $> P.ModuleName "Prelude"
                (a, ty) <- eval defaultModule exprText
                (, ty) <$> liftEval a
                
      it "should compile and evaluate literals" do
        result <- buildSingleExpressionWithPrelude @Integer False [prelude] "42"
        result `shouldBe` Right (42, P.tyInt)
          
      it "should compile and evaluate simple expressions" do
        result <- buildSingleExpressionWithPrelude @Integer False [prelude] "Prelude.identity 42"
        result `shouldBe` Right (42, P.tyInt)
          
      it "should compile and evaluate simple expressions with unqualified names from the default module" do
        result <- buildSingleExpressionWithPrelude @Integer True [prelude] "identity 42"
        result `shouldBe` Right (42, P.tyInt)
          
      describe "Golden expression tests" do
        testFiles <- map takeFileName <$> runIO (listDirectory "test-files")
        
        for_ testFiles \name -> do
          input <- runIO (IO.readFile ("test-files" </> name </> "input.purs"))
          outputOrError <- runIO $ buildSingleExpressionWithPrelude @Text True stdlib input
          actualOutput <-
            case outputOrError of
              Left err -> 
                pure $ Text.pack err 
              Right (result, _) -> 
                pure result
          it ("generates the correct output for test case " <> show name) $
            Golden {
              Test.Hspec.Golden.output = actualOutput,
              encodePretty = Text.unpack,
              writeToFile = IO.writeFile,
              readFromFile = IO.readFile,
              goldenFile = "test-files" </> name </> "golden",
              actualFile = Just ("test-files" </> name </> "actual"),
              failFirstTime = False
            }
    
data ExampleRecord1 = ExampleRecord1
  { foo :: Integer
  , bar :: Double
  , baz :: Bool
  , quux :: Text
  } deriving stock (Show, Eq, Generic) 
    deriving anyclass (ToValue m)
    deriving Arbitrary via GenericArbitrary ExampleRecord1
