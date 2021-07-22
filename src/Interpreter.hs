{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Interpreter
  ( Env
  , Value(..)
  , Constructor(..)
  , EvalException(..)
  , eval
  , apply
  , interpret
  , ToValue(..)
  , FromValue(..)
  , builtIn
  ) where

import Control.Exception (Exception, evaluate, throw, try)
import Control.Monad (guard, foldM, join, zipWithM)
import Control.Monad.Trans.Class (lift)
import Data.Align qualified as Align
import Data.Foldable (asum, fold)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These(..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.PureScript.CoreFn qualified as CoreFn
import Language.PureScript.Names (Ident(..), Qualified(..))
import Language.PureScript.Names qualified as Names
import Language.PureScript.PSString qualified as PSString

type Env = Map (Qualified Ident) Value

data Value
  = Object (HashMap Text Value)
  | Array (Vector Value)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  | Closure (Value -> Value)
  | Constructor Constructor
  
data Constructor = MkConstructor
  { ctorName :: Qualified (Names.ProperName 'Names.ConstructorName)
  , ctorApplied :: [Value]
  , ctorUnapplied :: [Ident]
  }
  
data EvalException 
  = UnknownIdent (Qualified Ident)
  | TypeMismatch Text
  | FieldNotFound Text
  | InexhaustivePatternMatch
  | NotSupported Text 
  -- ^ TODO: remove this
  | InvalidNumberOfArguments Int Int
  | UnsaturatedConstructorApplication
  | InvalidFieldName PSString.PSString
  | OtherError Text
  deriving Show

instance Exception EvalException
  
class ToValue a where
  toValue :: a -> Value
  
  default toValue :: (G.Generic a, GToObject (G.Rep a)) => a -> Value
  toValue = Object . gToObject . G.from
  
instance ToValue Value where
  toValue = id
  
instance ToValue Integer where
  toValue = Number . fromIntegral
  
instance ToValue Scientific where
  toValue = Number
  
instance ToValue Text where
  toValue = String
  
instance ToValue Bool where
  toValue = Bool
  
instance (FromValue a, ToValue b) => ToValue (a -> b) where
  toValue f = Closure (toValue . f . fromValue)

instance ToValue a => ToValue (Maybe a) where
  toValue = maybe Null toValue

instance ToValue a => ToValue (Vector a) where
  toValue = Array . fmap toValue
  
class FromValue a where
  fromValue :: Value -> a
  
  default fromValue :: (G.Generic a, GFromObject (G.Rep a)) => Value -> a
  fromValue = \case
    Object o -> G.to (gFromObject o)
    _ -> throw (TypeMismatch "object")
  
instance FromValue Value where
  fromValue = id
  
instance FromValue Text where
  fromValue = \case
    String s -> s
    _ -> throw (TypeMismatch "string")
  
instance FromValue Integer where
  fromValue = \case
    Number s
      | Right i <- floatingOrInteger s -> i
    _ -> throw (TypeMismatch "integer")
  
instance FromValue Scientific where
  fromValue = \case
    Number s -> s
    _ -> throw (TypeMismatch "number")
  
instance FromValue Bool where
  fromValue = \case
    Bool b -> b
    _ -> throw (TypeMismatch "boolean")
  
instance FromValue a => FromValue (Maybe a) where
  fromValue = \case
    Null -> Nothing
    v -> Just (fromValue v)
  
instance FromValue a => FromValue (Vector a) where
  fromValue = \case
    Array xs -> fmap fromValue xs
    _ -> throw (TypeMismatch "array")
  
instance (ToValue a, FromValue b) => FromValue (a -> b) where
  fromValue f a = fromValue (apply f (toValue a))
       
class GToObject f where
  gToObject :: f x -> HashMap Text Value
  
instance GToObject f => GToObject (G.M1 G.D t f) where
  gToObject = gToObject . G.unM1
  
instance GToObject f => GToObject (G.M1 G.C t f) where
  gToObject = gToObject . G.unM1
  
instance (GToObject f, GToObject g) => GToObject (f G.:*: g) where
  gToObject (f G.:*: g) = gToObject f <> gToObject g
    
instance 
    forall field u s l r a
     . ( KnownSymbol field
       , ToValue a
       ) 
    => GToObject 
         (G.M1 
           G.S
           (G.MetaSel 
             (Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gToObject (G.M1 (G.K1 a)) = do
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
       in HashMap.singleton field (toValue a)
  
class GFromObject f where
  gFromObject :: HashMap Text Value -> f x
  
instance GFromObject f => GFromObject (G.M1 G.D t f) where
  gFromObject = G.M1 . gFromObject
  
instance GFromObject f => GFromObject (G.M1 G.C t f) where
  gFromObject = G.M1 . gFromObject

instance 
    forall field u s l r a
     . ( KnownSymbol field
       , FromValue a
       ) 
    => GFromObject 
         (G.M1 
           G.S
           (G.MetaSel 
             (Just field) 
             u s l) 
            (G.K1 r a)) 
  where
    gFromObject o = do
      let field = Text.pack (symbolVal @field (Proxy :: Proxy field))
      case HashMap.lookup field o of
        Nothing -> throw (FieldNotFound field)
        Just v -> G.M1 (G.K1 (fromValue v))
  
instance (GFromObject f, GFromObject g) => GFromObject (f G.:*: g) where
  gFromObject o = gFromObject o G.:*: gFromObject o

builtIn :: ToValue a => Text -> a -> Interpreter.Env
builtIn name value =
  let qualName = Names.mkQualified (Names.Ident name) (Names.ModuleName "Main")
   in Map.singleton qualName $ toValue value
   
interpret :: FromValue a => Env -> CoreFn.Module ann -> a
interpret initialEnv CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleDecls } =
  let env = bind moduleName (Just moduleName) initialEnv (fmap void moduleDecls)
      mainFn = eval moduleName env (CoreFn.Var () (Qualified (Just moduleName) (Ident "main")))
   in fromValue mainFn

eval :: Names.ModuleName -> Env -> CoreFn.Expr () -> Value
eval mn env (CoreFn.Literal _ lit) =
  evalLit mn env lit
eval mn env (CoreFn.Accessor _ pss e) =
  let val = eval mn env e
      field = evalPSString pss
   in case val of
        Object o ->
          case HashMap.lookup field o of
            Just x -> x
            Nothing -> throw (FieldNotFound field)
eval mn env (CoreFn.Abs _ arg body) =
  Closure $ \v -> eval mn (Map.insert (Qualified Nothing arg) v env) body
eval mn env (CoreFn.App _ f x) =
  apply (eval mn env f) (eval mn env x)
eval mn env (CoreFn.Var _ name) =
  case Map.lookup name env of
    Nothing -> throw $ UnknownIdent name
    Just val -> val
eval mn env (CoreFn.Let _ binders body) = do
  let env' = bind mn Nothing env binders
  eval mn env' body
eval mn env (CoreFn.ObjectUpdate _ e updates) = do
  let val = eval mn env e
      updateOne 
        :: HashMap Text Value
        -> (PSString.PSString, CoreFn.Expr ())
        -> HashMap Text Value
      updateOne o (pss, new) = do
        let field = evalPSString pss
            newVal = eval mn env new
        HashMap.insert field newVal o
  case val of
    Object o -> Object (foldl updateOne o updates)
    _ -> throw (TypeMismatch "object")
eval mn env (CoreFn.Case _ args alts) = do
  let vals = fmap (eval mn env) args
      result = asum (map (match vals) alts)
  case result of
    Nothing -> throw InexhaustivePatternMatch
    Just (newEnv, matchedExpr) -> eval mn (newEnv <> env) matchedExpr
eval mn env (CoreFn.Constructor _ _tyName ctor fields) = 
  Constructor (MkConstructor (Qualified (Just mn) ctor) [] fields)

match :: [Value]
      -> CoreFn.CaseAlternative ()
      -> Maybe (Env, CoreFn.Expr ())
match vals (CoreFn.CaseAlternative binders expr) 
  | length vals == length binders = do
    newEnv <- fold <$> zipWithM matchOne vals binders
    case expr of
      Left guards -> throw (NotSupported "guards")
      Right e -> pure (newEnv, e)
  | otherwise = throw (InvalidNumberOfArguments (length vals) (length binders))

matchOne :: Value -> CoreFn.Binder () -> Maybe Env
matchOne _ (CoreFn.NullBinder _) = pure mempty
matchOne val (CoreFn.LiteralBinder _ lit) = matchLit val lit
matchOne val (CoreFn.VarBinder _ ident) = do
  pure (Map.singleton (Qualified Nothing ident) val)
matchOne val (CoreFn.NamedBinder _ ident b) = do
  env <- matchOne val b
  pure (Map.insert (Qualified Nothing ident) val env)
matchOne (Constructor (MkConstructor ctor vals [])) (CoreFn.ConstructorBinder _ _tyName ctor' bs) 
  | ctor == ctor'
  = fold <$> zipWithM matchOne vals bs
matchOne Constructor{} CoreFn.ConstructorBinder{} =
  throw UnsaturatedConstructorApplication
matchOne _ _ = Nothing

matchLit
  :: Value
  -> CoreFn.Literal (CoreFn.Binder ())
  -> Maybe Env
matchLit (Number n) (CoreFn.NumericLiteral (Left i)) 
  | fromIntegral i == n = pure mempty
matchLit (Number n) (CoreFn.NumericLiteral (Right d))
  | realToFrac d == n = pure mempty
matchLit (String s) (CoreFn.StringLiteral pss) = do
  guard (s == evalPSString pss)
  pure mempty
matchLit (String s) (CoreFn.CharLiteral chr)
  | s == Text.singleton chr = pure mempty
matchLit (Bool b) (CoreFn.BooleanLiteral b')
  | b == b' = pure mempty
matchLit (Array xs) (CoreFn.ArrayLiteral bs)
  | length xs == length bs
  = fold <$> zipWithM matchOne (Vector.toList xs) bs
matchLit (Object o) (CoreFn.ObjectLiteral bs) = do
  let vals = HashMap.fromList [ (t, (t, b)) | (pss, b) <- bs, let t = evalPSString pss ]
  let matchField :: These Value (Text, CoreFn.Binder ()) -> Maybe Env
      matchField This{} = pure mempty
      matchField (That (pss, _)) = throw (FieldNotFound pss)
      matchField (These val (_, b)) = matchOne val b
  fold <$> sequence (Align.alignWith matchField o vals)
matchLit _ _ = Nothing

evalPSString :: PSString.PSString -> Text
evalPSString pss = 
  case PSString.decodeString pss of
    Just field -> field
    _ -> throw (InvalidFieldName pss)

evalLit :: Names.ModuleName -> Env -> CoreFn.Literal (CoreFn.Expr ()) -> Value
evalLit mn env (CoreFn.NumericLiteral (Left int)) =
  Number (fromIntegral int)
evalLit mn env (CoreFn.NumericLiteral (Right dbl)) =
  Number (realToFrac dbl)
evalLit mn env (CoreFn.StringLiteral str) =
  String (evalPSString str)
evalLit mn env (CoreFn.CharLiteral chr) =
  String (Text.singleton chr)
evalLit mn env (CoreFn.BooleanLiteral b) =
  Bool b
evalLit mn env (CoreFn.ArrayLiteral xs) = do
  let vs = fmap (eval mn env) xs
  Array (Vector.fromList vs)
evalLit mn env (CoreFn.ObjectLiteral xs) = do
  let evalField (pss, e) = do
        let field = evalPSString pss
            val = eval mn env e
        (field, val)
      vs = fmap evalField xs
  Object (HashMap.fromList vs)

bind :: Names.ModuleName -> Maybe Names.ModuleName -> Env -> [CoreFn.Bind ()] -> Env
bind mn scope = foldl go where
  go :: Env -> CoreFn.Bind () -> Env
  go env (CoreFn.NonRec _ name e) = 
    Map.insert (Qualified scope name) (eval mn env e) env
  go env (CoreFn.Rec es) = 
    throw (NotSupported "recursive bindings")

apply :: Value -> Value -> Value
apply (Closure f) arg = f arg
apply (Constructor MkConstructor{ ctorName, ctorApplied, ctorUnapplied = hd : tl }) arg = do
  Constructor MkConstructor{ ctorName, ctorApplied = arg : ctorApplied, ctorUnapplied = tl }
apply _ _ = throw (TypeMismatch "closure")