{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.PureScript.Interpreter.Prelude where
  
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Data.Vector (Vector)
import Language.PureScript qualified as P
import Language.PureScript.Interpreter (EvalT, FFI(..), Value)
import Language.PureScript.Interpreter qualified as Interpreter
import Language.PureScript.Interpreter.FFI ((-->), array, boolean, forAll, string)

prelude :: forall m. MonadFix m => FFI m
prelude = 
    FFI (P.ModuleName "Prelude")
      [ ( P.Ident "map"
        , forAll \a -> forAll \b -> (a --> b) --> array a --> array b
        , Interpreter.toValue _map
        )
      , ( P.Ident "append"
        , forAll \a -> array a --> array a --> array a
        , Interpreter.toValue append
        )
      , ( P.Ident "appendString"
        , string --> string --> string
        , Interpreter.toValue appendString
        )
      , ( P.Ident "not"
        , boolean --> boolean
        , Interpreter.toValue _not
        )
      ]
  where
    _map :: (Value m -> EvalT m (Value m))
         -> Vector (Value m)
         -> EvalT m (Vector (Value m))
    _map = traverse
    
    append :: Vector (Value m)
           -> Vector (Value m)
           -> EvalT m (Vector (Value m))
    append xs ys = pure (xs <> ys)
  
    appendString :: Text -> Text -> EvalT m Text
    appendString xs ys = pure (xs <> ys)
    
    _not :: Bool -> EvalT m Bool
    _not b = pure (not b)