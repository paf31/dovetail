{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core where

import Codec.Serialise qualified as Codec
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (decodeStrict)
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed
import Data.Foldable (fold, traverse_)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Dovetail
import Language.PureScript.CoreFn.FromJSON qualified as FromJSON
import System.FilePath ((</>))

import Dovetail.Core.Control.Apply qualified as Control.Apply
import Dovetail.Core.Control.Bind qualified as Control.Bind
import Dovetail.Core.Control.Extend qualified as Control.Extend
import Dovetail.Core.Control.Monad.ST.Internal qualified as Control.Monad.ST.Internal
import Dovetail.Core.Data.Array qualified as Data.Array
import Dovetail.Core.Data.Array.NonEmpty.Internal qualified as Data.Array.NonEmpty.Internal
import Dovetail.Core.Data.Array.ST qualified as Data.Array.ST
import Dovetail.Core.Data.Array.ST.Partial qualified as Data.Array.ST.Partial
import Dovetail.Core.Data.Bounded qualified as Data.Bounded
import Dovetail.Core.Data.Enum qualified as Data.Enum
import Dovetail.Core.Data.Eq qualified as Data.Eq
import Dovetail.Core.Data.EuclideanRing qualified as Data.EuclideanRing
import Dovetail.Core.Data.Foldable qualified as Data.Foldable
import Dovetail.Core.Data.Function.Uncurried qualified as Data.Function.Uncurried
import Dovetail.Core.Data.Functor qualified as Data.Functor
import Dovetail.Core.Data.FunctorWithIndex qualified as Data.FunctorWithIndex
import Dovetail.Core.Data.HeytingAlgebra qualified as Data.HeytingAlgebra
import Dovetail.Core.Data.Int qualified as Data.Int
import Dovetail.Core.Data.Int.Bits qualified as Data.Int.Bits
import Dovetail.Core.Data.Lazy qualified as Data.Lazy
import Dovetail.Core.Data.Number qualified as Data.Number
import Dovetail.Core.Data.Number.Format qualified as Data.Number.Format
import Dovetail.Core.Data.Ord qualified as Data.Ord
import Dovetail.Core.Data.Ring qualified as Data.Ring
import Dovetail.Core.Data.Semigroup qualified as Data.Semigroup
import Dovetail.Core.Data.Semiring qualified as Data.Semiring
import Dovetail.Core.Data.Show qualified as Data.Show
import Dovetail.Core.Data.Show.Generic qualified as Data.Show.Generic
import Dovetail.Core.Data.String.CodePoints qualified as Data.String.CodePoints
import Dovetail.Core.Data.String.CodeUnits qualified as Data.String.CodeUnits
import Dovetail.Core.Data.String.Common qualified as Data.String.Common
import Dovetail.Core.Data.String.Regex qualified as Data.String.Regex
import Dovetail.Core.Data.String.Unsafe qualified as Data.String.Unsafe
import Dovetail.Core.Data.Symbol qualified as Data.Symbol
import Dovetail.Core.Data.Traversable qualified as Data.Traversable
import Dovetail.Core.Data.Unfoldable qualified as Data.Unfoldable
import Dovetail.Core.Data.Unfoldable1 qualified as Data.Unfoldable1
import Dovetail.Core.Data.Unit qualified as Data.Unit
import Dovetail.Core.Effect qualified as Effect
import Dovetail.Core.Effect.Console qualified as Effect.Console
import Dovetail.Core.Effect.Exception qualified as Effect.Exception
import Dovetail.Core.Effect.Random qualified as Effect.Random
import Dovetail.Core.Effect.Ref qualified as Effect.Ref
import Dovetail.Core.Effect.Uncurried qualified as Effect.Uncurried
import Dovetail.Core.Effect.Unsafe qualified as Effect.Unsafe
import Dovetail.Core.Math qualified as Math
import Dovetail.Core.Partial qualified as Partial
import Dovetail.Core.Partial.Unsafe qualified as Partial.Unsafe
import Dovetail.Core.Record.Builder qualified as Record.Builder
import Dovetail.Core.Record.Unsafe qualified as Record.Unsafe
import Dovetail.Core.Record.Unsafe.Union qualified as Record.Unsafe.Union
import Dovetail.Core.Test.Assert qualified as Test.Assert
import Dovetail.Core.Unsafe.Coerce qualified as Unsafe.Coerce
  
pursFiles :: [(FilePath, BS.ByteString)]
pursFiles = $(makeRelativeToProject "purs/output" >>= embedDir)
  
core :: forall ctx. Typeable ctx => Interpret ctx ()
core = do
    loadEnv env
    traverse_ buildOne modules
  where
    buildOne moduleName = do
      liftIO . putStrLn $ moduleName
      let externsFile = BL.fromStrict . fromJust $ lookup (moduleName </> "externs.cbor") pursFiles
          coreFnFile  = fromJust . decodeStrict . fromJust $ lookup (moduleName </> "corefn.json") pursFiles
          readCoreFn = either error snd . parseEither FromJSON.moduleFromJSON
      buildCoreFn (Codec.deserialise externsFile) (readCoreFn coreFnFile)
    
    modules =
      [ "Type.Proxy"
      , "Type.Data.RowList"
      , "Type.Data.Row"
      , "Record.Unsafe"
      , "Data.NaturalTransformation"
      , "Data.Boolean"
      , "Control.Semigroupoid"
      , "Data.Symbol"
      , "Control.Category"
      , "Data.Show"
      , "Data.Unit"
      , "Data.Void"
      , "Data.HeytingAlgebra"
      , "Data.Semiring"
      , "Data.Semigroup"
      , "Data.Generic.Rep"
      , "Data.Ring"
      , "Data.BooleanAlgebra"
      , "Data.Eq"
      , "Data.CommutativeRing"
      , "Data.Ordering"
      , "Data.EuclideanRing"
      , "Data.Ord"
      , "Data.DivisionRing"
      , "Data.Field"
      , "Data.Monoid"
      , "Data.Function"
      , "Data.Bounded"
      , "Data.Functor"
      , "Control.Apply"
      , "Data.Monoid.Generic"
      , "Data.Bounded.Generic"
      , "Control.Applicative"
      , "Control.Bind"
      , "Control.Monad"
      , "Prelude"
      , "Data.Semiring.Generic"
      , "Data.Monoid.Additive"
      , "Data.Monoid.Dual"
      , "Data.Monoid.Multiplicative"
      , "Data.Ord.Generic"
      , "Data.HeytingAlgebra.Generic"
      , "Data.Ring.Generic"
      , "Data.Show.Generic"
      , "Data.Monoid.Conj"
      , "Data.Semigroup.Generic"
      , "Data.Semigroup.First"
      , "Data.Semigroup.Last"
      , "Data.Eq.Generic"
      , "Data.Monoid.Disj"
      , "Data.Monoid.Endo"
      , "Unsafe.Coerce"
      , "Safe.Coerce"
      , "Data.Newtype"
      , "Control.Lazy"
      , "Control.Extend"
      , "Control.Alt"
      , "Control.Plus"
      , "Control.Comonad"
      , "Control.Alternative"
      , "Control.MonadZero"
      , "Control.MonadPlus"
      , "Data.Monoid.Alternate"
      , "Data.Functor.Invariant"
      , "Data.Const"
      , "Data.Maybe"
      , "Data.Maybe.Last"
      , "Data.Maybe.First"
      , "Data.Either"
      , "Data.Either.Nested"
      , "Data.Either.Inject"
      , "Data.Tuple"
      , "Data.Tuple.Nested"
      , "Data.Bifunctor"
      , "Control.Biapply"
      , "Control.Biapplicative"
      , "Data.Bifunctor.Join"
      , "Data.Functor.Contravariant"
      , "Data.Op"
      , "Data.Predicate"
      , "Data.Comparison"
      , "Data.Equivalence"
      , "Data.Divide"
      , "Data.Divisible"
      , "Data.Decide"
      , "Data.Decidable"
      , "Data.Identity"
      , "Type.Equality"
      , "Data.Distributive"
      , "Data.Exists"
      , "Data.Profunctor"
      , "Data.Profunctor.Closed"
      , "Data.Profunctor.Join"
      , "Data.Profunctor.Split"
      , "Data.Profunctor.Strong"
      , "Data.Profunctor.Costrong"
      , "Data.Profunctor.Cochoice"
      , "Data.Profunctor.Choice"
      , "Data.Profunctor.Star"
      , "Data.Functor.App"
      , "Data.Functor.Compose"
      , "Data.Functor.Coproduct"
      , "Data.Functor.Product"
      , "Data.Functor.Costar"
      , "Data.Functor.Joker"
      , "Data.Functor.Clown"
      , "Data.Functor.Flip"
      , "Data.Functor.Product2"
      , "Data.Functor.Coproduct.Inject"
      , "Data.Functor.Coproduct.Nested"
      , "Data.Functor.Product.Nested"
      , "Data.Ord.Down"
      , "Data.Ord.Min"
      , "Data.Ord.Max"
      , "Data.Traversable.Accum"
      , "Data.Traversable.Accum.Internal"
      , "Data.FunctorWithIndex"
      , "Data.Foldable"
      , "Data.Bifoldable"
      , "Data.FoldableWithIndex"
      , "Data.Traversable"
      , "Data.Semigroup.Foldable"
      , "Data.Semigroup.Traversable"
      , "Data.Bitraversable"
      , "Data.TraversableWithIndex"
      , "Partial"
      , "Partial.Unsafe"
      , "Data.Unfoldable1"
      , "Data.Unfoldable"
      , "Data.NonEmpty"
      , "Effect"
      , "Effect.Unsafe"
      , "Effect.Uncurried"
      , "Effect.Class"
      , "Effect.Ref"
      , "Control.Monad.Rec.Class"
      , "Control.Monad.ST.Internal"
      , "Control.Monad.ST"
      , "Control.Monad.ST.Global"
      , "Control.Monad.ST.Ref"
      , "Control.Monad.ST.Class"
      , "Data.Array.ST"
      , "Data.Array.ST.Partial"
      , "Data.Array.ST.Iterator"
      , "Data.Array.NonEmpty.Internal"
      , "Data.Array"
      , "Data.Array.Partial"
      , "Data.Array.NonEmpty"
      , "Effect.Console"
      , "Effect.Class.Console"
      , "Test.Assert"
      , "Data.Lazy"
      , "Data.List.Lazy.Types"
      , "Data.List.Types"
      , "Data.List.Internal"
      , "Data.List.Lazy"
      , "Data.List"
      , "Data.List.ZipList"
      , "Data.List.Lazy.NonEmpty"
      , "Data.List.Partial"
      , "Data.List.NonEmpty"
      , "Data.CatQueue"
      , "Data.CatList"
      , "Control.Monad.Gen.Class"
      , "Control.Monad.Gen"
      , "Control.Monad.Gen.Common"
      , "Data.Enum"
      , "Data.Enum.Generic"
      , "Data.Enum.Gen"
      , "Data.Function.Uncurried"
      , "Math"
      , "Data.Number.Approximate"
      , "Data.Number.Format"
      , "Data.Number"
      , "Data.Int.Bits"
      , "Data.Int"
      , "Data.Map.Internal"
      , "Data.Set"
      , "Data.Map"
      , "Data.Set.NonEmpty"
      , "Data.Map.Gen"
      -- "Data.Time.Duration"
      -- "Data.Time.Duration.Gen"
      -- "Data.Time.Component"
      -- "Data.Date.Component"
      -- "Data.Time.Component.Gen"
      -- "Data.Time"
      -- "Data.Date.Component.Gen"
      -- "Data.Date"
      -- "Data.Time.Gen"
      -- "Data.Date.Gen"
      -- "Data.DateTime"
      -- "Data.DateTime.Gen"
      -- "Data.DateTime.Instant"
      -- "Data.Interval.Duration"
      -- "Data.Interval"
      -- "Data.Interval.Duration.Iso"
      , "Effect.Exception"
      , "Effect.Exception.Unsafe"
      , "Data.String.Unsafe"
      , "Data.String.Pattern"
      , "Data.String.Common"
      , "Data.String.CodeUnits"
      , "Data.Char"
      , "Data.Char.Gen"
      , "Data.String.Gen"
      , "Data.String.CodePoints"
      , "Data.String"
      , "Data.String.NonEmpty.Internal"
      , "Data.String.CaseInsensitive"
      , "Data.String.Regex.Flags"
      , "Data.String.NonEmpty.CodeUnits"
      , "Data.String.Regex"
      , "Data.String.NonEmpty.CodePoints"
      , "Data.String.Regex.Unsafe"
      , "Data.String.NonEmpty"
      , "Data.String.NonEmpty.CaseInsensitive"
      , "Control.Comonad.Trans.Class"
      , "Control.Monad.Cont.Class"
      , "Control.Monad.Trans.Class"
      , "Control.Monad.Reader.Class"
      , "Control.Comonad.Traced.Trans"
      , "Control.Comonad.Store.Trans"
      , "Control.Monad.State.Class"
      , "Control.Monad.Writer.Class"
      , "Control.Comonad.Traced.Class"
      , "Control.Monad.Cont.Trans"
      , "Control.Comonad.Traced"
      , "Control.Monad.Error.Class"
      , "Control.Monad.Cont"
      , "Control.Monad.Maybe.Trans"
      , "Control.Monad.RWS.Trans"
      , "Control.Monad.Reader.Trans"
      , "Control.Monad.Except.Trans"
      , "Control.Monad.Writer.Trans"
      , "Control.Monad.State.Trans"
      , "Control.Monad.Reader"
      , "Control.Monad.Except"
      , "Control.Monad.Writer"
      , "Control.Monad.State"
      , "Control.Monad.RWS"
      , "Control.Comonad.Env.Trans"
      , "Control.Monad.Identity.Trans"
      , "Control.Comonad.Env.Class"
      , "Control.Comonad.Store.Class"
      , "Control.Comonad.Env"
      , "Control.Comonad.Store"
      , "Control.Monad.List.Trans"
      -- "Foreign"
      -- "Foreign.Keys"
      -- "Foreign.Index"
      , "Type.Function"
      , "Type.Row"
      , "Type.Data.Boolean"
      , "Type.Data.Ordering"
      , "Type.Data.Symbol"
      , "Type.RowList"
      , "Type.Prelude"
      , "Type.Row.Homogeneous"
      -- "Foreign.Object.ST"
      -- "Foreign.Object"
      -- "Foreign.Object.Unsafe"
      -- "Foreign.Object.ST.Unsafe"
      -- "Foreign.Object.Gen"
      , "Data.Yoneda"
      , "Data.Coyoneda"
      , "Control.Monad.Free"
      , "Control.Comonad.Cofree"
      , "Control.Monad.Trampoline"
      , "Control.Monad.Free.Class"
      , "Control.Comonad.Cofree.Class"
      , "Data.Graph"
      , "Effect.Random"
      , "Random.LCG"
      , "Control.Parallel.Class"
      , "Control.Parallel"
      , "PSCI.Support"
      , "Record.Unsafe.Union"
      , "Record.Builder"
      , "Record"
      , "Test.QuickCheck.Gen"
      , "Test.QuickCheck.Arbitrary"
      , "Test.QuickCheck"
      , "Data.Semiring.Free"
      , "Data.Validation.Semigroup"
      , "Data.Validation.Semiring"
      ]

env :: forall ctx. Typeable ctx => Env ctx
env = 
  fold
    [ Control.Apply.env
    , Control.Bind.env
    , Control.Extend.env
    , Control.Monad.ST.Internal.env
    , Data.Array.env
    , Data.Array.NonEmpty.Internal.env
    , Data.Array.ST.env
    , Data.Array.ST.Partial.env
    , Data.Bounded.env
    , Data.Enum.env
    , Data.Eq.env
    , Data.EuclideanRing.env
    , Data.Foldable.env
    , Data.Function.Uncurried.env
    , Data.Functor.env
    , Data.FunctorWithIndex.env
    , Data.HeytingAlgebra.env
    , Data.Int.env
    , Data.Int.Bits.env
    , Data.Lazy.env
    , Data.Number.env
    , Data.Number.Format.env
    , Data.Ord.env
    , Data.Ring.env
    , Data.Semigroup.env
    , Data.Semiring.env
    , Data.Show.env
    , Data.Show.Generic.env
    , Data.String.CodePoints.env
    , Data.String.CodeUnits.env
    , Data.String.Common.env
    , Data.String.Regex.env
    , Data.String.Unsafe.env
    , Data.Symbol.env
    , Data.Traversable.env
    , Data.Unfoldable.env
    , Data.Unfoldable1.env
    , Data.Unit.env
    , Effect.env
    , Effect.Console.env
    , Effect.Exception.env
    , Effect.Random.env
    , Effect.Ref.env
    , Effect.Uncurried.env
    , Effect.Unsafe.env
    , Math.env
    , Partial.env
    , Partial.Unsafe.env
    , Record.Builder.env
    , Record.Unsafe.env
    , Record.Unsafe.Union.env
    , Test.Assert.env
    , Unsafe.Coerce.env
    ]