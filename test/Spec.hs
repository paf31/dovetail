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
  
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Functor (($>))
import Data.Functor.Identity
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
            first (renderInterpretError renderOpts) $
              runInterpret do
                ffi prelude
                m <- build moduleText
                evalMain (CoreFn.moduleName m)
                
      it "should build single modules from source" $
        fmap (first (renderEvaluationError renderOpts) . runEval) 
          (buildSingleModuleWithPrelude @(Eval Integer) 
            "module Main where main = 42")
          `shouldBe` Right (Right 42)
      
      it "should support imports" $
        fmap (first (renderEvaluationError renderOpts) . runEval) 
          (buildSingleModuleWithPrelude @(Eval Integer) 
            "module Main where\n\
            \import Prelude\n\
            \main = identity 42")
          `shouldBe` Right (Right 42)
      
      it "should support returning functions" $
        fmap (first (renderEvaluationError renderOpts) . runEval . ($ "testing")) 
          (buildSingleModuleWithPrelude @(Text -> Eval Text) 
            "module Main where\n\
            \import Prelude\n\
            \main x = x")
          `shouldBe` Right (Right "testing")
      
      it "should support records" $
        fmap (first (renderEvaluationError renderOpts) . runEval . ($ "testing")) 
          (buildSingleModuleWithPrelude @(Text -> Eval ExampleRecord1) 
            "module Main where\n\
            \import Prelude\n\
            \main x = { foo: 42, bar: 1.0, baz: true, quux: x }")
          `shouldBe` Right (Right (ExampleRecord1 42 1.0 True "testing"))

      it "should support mixing module and expression evaluation" do
        let x = runInterpret do
                  traverse_ ffi stdlib
                  let moduleText =
                        "module Main where\n\
                        \import Prelude.Array\n\
                        \main = map _.foo"
                  CoreFn.Module { CoreFn.moduleName = mn } <- build moduleText
                  runEval . fst <$> eval (Just mn) "main [{ foo: 42 }]"
        fmap (first (renderEvaluationError renderOpts)) (first (renderInterpretError renderOpts) x)
          `shouldBe` Right (Right (Vector.fromList [42 :: Integer]))
          
      let buildSingleExpressionWithPrelude 
            :: forall a
             . ToValueRHS Identity a
            => Bool
            -> [FFI Identity]
            -> Text
            -> Either String (a, P.SourceType)
          buildSingleExpressionWithPrelude importPreludeUnqualified ffiModules exprText =
            first (renderInterpretError renderOpts) $
              runInterpret do
                traverse_ ffi ffiModules
                let defaultModule = guard importPreludeUnqualified $> P.ModuleName "Prelude"
                eval defaultModule exprText
                
      it "should compile and evaluate literals" $
        fmap (first (first (renderEvaluationError renderOpts) . runEval)) 
          (buildSingleExpressionWithPrelude @(Eval Integer) False [prelude] "42")
          `shouldBe` Right (Right 42, P.tyInt)
          
      it "should compile and evaluate simple expressions" $
        fmap (first (first (renderEvaluationError renderOpts) . runEval)) 
          (buildSingleExpressionWithPrelude @(Eval Integer) False [prelude] "Prelude.identity 42")
          `shouldBe` Right (Right 42, P.tyInt)
          
      it "should compile and evaluate simple expressions with unqualified names from the default module" $
        fmap (first (first (renderEvaluationError renderOpts) . runEval)) 
          (buildSingleExpressionWithPrelude @(Eval Integer) True [prelude] "identity 42")
          `shouldBe` Right (Right 42, P.tyInt)
          
      describe "Golden expression tests" do
        testFiles <- map takeFileName <$> runIO (listDirectory "test-files")
        
        for_ testFiles \name -> do
          input <- runIO (IO.readFile ("test-files" </> name </> "input.purs"))
          let actualOutput = 
                case buildSingleExpressionWithPrelude @(Eval Text) True stdlib input of
                  Left err -> 
                    Text.pack err 
                  Right (value, _) -> 
                    case runEval value of
                      Left err ->
                        Text.pack (renderEvaluationError renderOpts err)
                      Right result ->
                        result
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
