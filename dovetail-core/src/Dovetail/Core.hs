{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dovetail.Core 
  ( 
  -- ** Building core libraries
    CoreBuild(..)
  , buildModules
  
  -- ** Package collections
  , allPackages
  , minimal
  
  -- ** Individual packages
  , arrays
  , assert
  , bifunctors
  , catenableLists
  , console
  , _const
  , contravariant
  , control
  , distributive
  , effect
  , _either
  , enums
  , exceptions
  , exists
  , foldableTraversable
  , orders
  , free
  , functions
  , functors
  , gen
  , graphs
  , identity
  , integers
  , invariant
  , lazy
  , lcg
  , lists
  , math
  , _maybe
  , _newtype
  , nonempty
  , numbers
  , orderedCollections
  , typelevelPrelude
  , parallel
  , partial
  , prelude
  , profunctor
  , psciSupport
  , quickcheck
  , random
  , record
  , refs
  , safeCoerce
  , semirings
  , st
  , strings
  , tailrec
  , transformers
  , tuples
  , typeEquality
  , unfoldable
  , unsafeCoerce
  , validation
  ) where

import Codec.Serialise qualified as Codec
import Data.Aeson (decodeStrict)
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed
import Data.Foldable (fold, traverse_)
import Data.Maybe (fromJust, maybeToList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Dovetail
import Dovetail.Core.Modules (modules)
import Language.Haskell.TH.Syntax qualified as TH
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
  
$(concat <$> for modules \moduleName -> do
    externsFile <- makeRelativeToProject ("purs/output/" </> Text.unpack moduleName </> "externs.cbor")
    coreFnFile  <- makeRelativeToProject ("purs/output/" </> Text.unpack moduleName </> "corefn.json")
    let declName = TH.mkName . Text.unpack $ "_" <> Text.replace "." "_" moduleName
    declType <- [t| (Text, (BS.ByteString, BS.ByteString)) |]
    declExpr <- [e| (moduleName, ($(embedFile externsFile), $(embedFile coreFnFile))) |]
    pure [ TH.SigD declName declType 
         , TH.ValD (TH.VarP declName) (TH.NormalB declExpr) []
         ]
 )
 
data CoreBuild ctx = CoreBuild
  { buildInputs :: HashMap Text (BS.ByteString, BS.ByteString)
  , env :: Env ctx
  }
  
instance Semigroup (CoreBuild ctx) where
  CoreBuild bi1 e1 <> CoreBuild bi2 e2 = CoreBuild (bi1 <> bi2) (e1 <> e2)

instance Monoid (CoreBuild ctx) where
  mempty = CoreBuild mempty mempty

buildModules :: forall ctx. Typeable ctx => CoreBuild ctx -> Interpret ctx ()
buildModules bld = do
    loadEnv (env bld)
    let orderedInputs = [ (mn, inputs)
                        | mn <- modules
                        , inputs <- maybeToList (HashMap.lookup mn (buildInputs bld))
                        ]
    traverse_ buildOne orderedInputs
  where
    buildOne (_, (externs, corefn)) = do
      let readCoreFn = either error snd . parseEither FromJSON.moduleFromJSON
      buildCoreFn 
        (Codec.deserialise (BL.fromStrict externs))
        (readCoreFn (fromJust (decodeStrict corefn)))

allPackages :: Typeable ctx => CoreBuild ctx
allPackages = fold
  [ arrays
  , assert
  , bifunctors
  , catenableLists
  , console
  , _const
  , contravariant
  , control
  , distributive
  , effect
  , _either
  , enums
  , exceptions
  , exists
  , foldableTraversable
  , orders
  , free
  , functions
  , functors
  , gen
  , graphs
  , identity
  , integers
  , invariant
  , lazy
  , lcg
  , lists
  , math
  , _maybe
  , _newtype
  , nonempty
  , numbers
  , orderedCollections
  , typelevelPrelude
  , parallel
  , partial
  , prelude
  , profunctor
  , psciSupport
  , quickcheck
  , random
  , record
  , refs
  , safeCoerce
  , semirings
  , st
  , strings
  , tailrec
  , transformers
  , tuples
  , typeEquality
  , unfoldable
  , unsafeCoerce
  , validation
  ]
  
-- | A smaller package collection which is the transitive closure of
-- the arrays, strings, integers, numbers and math packages.
minimal :: Typeable ctx => CoreBuild ctx
minimal = fold
  [ arrays
  , bifunctors
  , console
  , _const
  , contravariant
  , control
  , distributive
  , effect
  , _either
  , enums
  , exists
  , foldableTraversable
  , functions
  , functors
  , gen
  , identity
  , integers
  , invariant
  , math
  , _maybe
  , _newtype
  , nonempty
  , numbers
  , orders
  , partial
  , prelude
  , profunctor
  , psciSupport
  , refs
  , safeCoerce
  , st
  , strings
  , tailrec
  , tuples
  , typeEquality
  , unfoldable
  , unsafeCoerce
  ]


arrays :: Typeable ctx => CoreBuild ctx
arrays = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Array_NonEmpty_Internal
      , _Data_Array_NonEmpty
      , _Data_Array_Partial
      , _Data_Array_ST_Partial
      , _Data_Array_ST_Iterator
      , _Data_Array_ST
      , _Data_Array
      ]
  , env = fold
      [ Data.Array.env
      , Data.Array.NonEmpty.Internal.env
      , Data.Array.ST.env
      , Data.Array.ST.Partial.env
      ]
  }

assert :: Typeable ctx => CoreBuild ctx
assert = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Test_Assert
      ]
  , env = Test.Assert.env
  }

bifunctors :: Typeable ctx => CoreBuild ctx
bifunctors = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Bifunctor
      , _Data_Bifunctor_Join
      , _Control_Biapplicative
      , _Control_Biapply
      ]
  , env = mempty
  }

catenableLists :: Typeable ctx => CoreBuild ctx
catenableLists = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_CatList
      , _Data_CatQueue
      ]
  , env = mempty
  }

console :: Typeable ctx => CoreBuild ctx
console = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Effect_Console
      , _Effect_Class_Console
      ]
  , env = Effect.Console.env
  }

_const :: Typeable ctx => CoreBuild ctx
_const = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Const
      ]
  , env = mempty
  }

contravariant :: Typeable ctx => CoreBuild ctx
contravariant = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Functor_Contravariant
      , _Data_Divisible
      , _Data_Op
      , _Data_Decidable
      , _Data_Equivalence
      , _Data_Comparison
      , _Data_Predicate
      , _Data_Divide
      , _Data_Decide
      ]
  , env = mempty
  }

control :: Typeable ctx => CoreBuild ctx
control = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Monoid_Alternate
      , _Control_Comonad
      , _Control_MonadZero
      , _Control_Alt
      , _Control_MonadPlus
      , _Control_Lazy
      , _Control_Extend
      , _Control_Alternative
      , _Control_Plus
      ]
  , env = Control.Extend.env
  }

distributive :: Typeable ctx => CoreBuild ctx
distributive = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Distributive
      ]
  , env = mempty
  }

effect :: Typeable ctx => CoreBuild ctx
effect = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Effect
      , _Effect_Class
      , _Effect_Uncurried
      , _Effect_Unsafe
      ]
  , env = fold
      [ Effect.env
      , Effect.Uncurried.env
      , Effect.Unsafe.env
      ]
  }

_either :: Typeable ctx => CoreBuild ctx
_either = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Either_Inject
      , _Data_Either_Nested
      , _Data_Either
      ]
  , env = mempty
  }

enums :: Typeable ctx => CoreBuild ctx
enums = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Enum_Gen
      , _Data_Enum_Generic
      , _Data_Enum
      ]
  , env = mempty
  }

exceptions :: Typeable ctx => CoreBuild ctx
exceptions = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Effect_Exception
      , _Effect_Exception_Unsafe
      ]
  , env = Effect.Exception.env
  }

exists :: Typeable ctx => CoreBuild ctx
exists = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Exists
      ]
  , env = mempty
  }

foldableTraversable :: Typeable ctx => CoreBuild ctx
foldableTraversable = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Traversable
      , _Data_TraversableWithIndex
      , _Data_Traversable_Accum
      , _Data_Traversable_Accum_Internal
      , _Data_Bifoldable
      , _Data_FunctorWithIndex
      , _Data_Bitraversable
      , _Data_Foldable
      , _Data_FoldableWithIndex
      , _Data_Semigroup_Traversable
      , _Data_Semigroup_Foldable
      ]
  , env = fold
      [ Data.Foldable.env
      , Data.Functor.env
      , Data.FunctorWithIndex.env
      , Data.Traversable.env
      ]
  }

orders :: Typeable ctx => CoreBuild ctx
orders = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Ord_Max
      , _Data_Ord_Min
      , _Data_Ord_Down
      ]
  , env = mempty
  }

free :: Typeable ctx => CoreBuild ctx
free = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Yoneda
      , _Data_Coyoneda
      , _Control_Comonad_Cofree_Class
      , _Control_Comonad_Cofree
      , _Control_Monad_Free_Class
      , _Control_Monad_Free
      , _Control_Monad_Trampoline
      ]
  , env = mempty
  }

functions :: Typeable ctx => CoreBuild ctx
functions = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Function_Uncurried
      ]
  , env = Data.Function.Uncurried.env
  }

functors :: Typeable ctx => CoreBuild ctx
functors = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Functor_Coproduct_Inject
      , _Data_Functor_Coproduct_Nested
      , _Data_Functor_Costar
      , _Data_Functor_Flip
      , _Data_Functor_Product
      , _Data_Functor_Joker
      , _Data_Functor_Product_Nested
      , _Data_Functor_Compose
      , _Data_Functor_Coproduct
      , _Data_Functor_Product2
      , _Data_Functor_App
      , _Data_Functor_Clown
      ]
  , env = mempty
  }

gen :: Typeable ctx => CoreBuild ctx
gen = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Control_Monad_Gen
      , _Control_Monad_Gen_Class
      , _Control_Monad_Gen_Common
      ]
  , env = mempty
  }

graphs :: Typeable ctx => CoreBuild ctx
graphs = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Graph
      ]
  , env = mempty
  }

identity :: Typeable ctx => CoreBuild ctx
identity = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Identity
      ]
  , env = mempty
  }

integers :: Typeable ctx => CoreBuild ctx
integers = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Int
      , _Data_Int_Bits
      ]
  , env = fold
      [ Data.Int.env
      , Data.Int.Bits.env
      ]
  }

invariant :: Typeable ctx => CoreBuild ctx
invariant = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Functor_Invariant
      ]
  , env = mempty
  }

lazy :: Typeable ctx => CoreBuild ctx
lazy = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Lazy
      ]
  , env = Data.Lazy.env
  }

lcg :: Typeable ctx => CoreBuild ctx
lcg = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Random_LCG
      ]
  , env = mempty
  }

lists :: Typeable ctx => CoreBuild ctx
lists = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_List
      , _Data_List_NonEmpty
      , _Data_List_Internal
      , _Data_List_Partial
      , _Data_List_ZipList
      , _Data_List_Lazy
      , _Data_List_Types
      , _Data_List_Lazy_NonEmpty
      , _Data_List_Lazy_Types
      ]
  , env = mempty
  }

math :: Typeable ctx => CoreBuild ctx
math = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Math
      ]
  , env = Math.env
  }

_maybe :: Typeable ctx => CoreBuild ctx
_maybe = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Maybe
      , _Data_Maybe_Last
      , _Data_Maybe_First
      ]
  , env = mempty
  }

_newtype :: Typeable ctx => CoreBuild ctx
_newtype = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Newtype
      ]
  , env = mempty
  }

nonempty :: Typeable ctx => CoreBuild ctx
nonempty = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_NonEmpty
      ]
  , env = mempty
  }

numbers :: Typeable ctx => CoreBuild ctx
numbers = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Number_Format
      , _Data_Number_Approximate
      , _Data_Number
      ]
  , env = fold
      [ Data.Number.env
      , Data.Number.Format.env
      ]
  }

orderedCollections :: Typeable ctx => CoreBuild ctx
orderedCollections = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Map
      , _Data_Map_Gen
      , _Data_Map_Internal
      , _Data_Set_NonEmpty
      , _Data_Set
      ]
  , env = mempty
  }

typelevelPrelude :: Typeable ctx => CoreBuild ctx
typelevelPrelude = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Type_RowList
      , _Type_Row
      , _Type_Row_Homogeneous
      , _Type_Data_Symbol
      , _Type_Data_Boolean
      , _Type_Data_Ordering
      , _Type_Prelude
      , _Type_Function
      ]
  , env = mempty
  }

parallel :: Typeable ctx => CoreBuild ctx
parallel = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Control_Parallel_Class
      , _Control_Parallel
      ]
  , env = mempty
  }

partial :: Typeable ctx => CoreBuild ctx
partial = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Partial_Unsafe
      , _Partial
      ]
  , env = fold
      [ Partial.env
      , Partial.Unsafe.env
      ]
  }

prelude :: Typeable ctx => CoreBuild ctx
prelude = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Record_Unsafe
      , _Type_Proxy
      , _Type_Data_RowList
      , _Type_Data_Row
      , _Data_BooleanAlgebra
      , _Data_Show
      , _Data_CommutativeRing
      , _Data_Monoid_Disj
      , _Data_Monoid_Multiplicative
      , _Data_Monoid_Additive
      , _Data_Monoid_Dual
      , _Data_Monoid_Endo
      , _Data_Monoid_Generic
      , _Data_Monoid_Conj
      , _Data_Unit
      , _Data_Void
      , _Data_Eq_Generic
      , _Data_Show_Generic
      , _Data_Ring
      , _Data_NaturalTransformation
      , _Data_Monoid
      , _Data_Semiring_Generic
      , _Data_Semigroup
      , _Data_Semigroup_Last
      , _Data_Semigroup_Generic
      , _Data_Semigroup_First
      , _Data_Bounded
      , _Data_Symbol
      , _Data_Bounded_Generic
      , _Data_Generic_Rep
      , _Data_Boolean
      , _Data_Eq
      , _Data_EuclideanRing
      , _Data_Ord
      , _Data_Ring_Generic
      , _Data_Ord_Generic
      , _Data_Ordering
      , _Data_Field
      , _Data_Functor
      , _Data_HeytingAlgebra_Generic
      , _Data_HeytingAlgebra
      , _Data_Function
      , _Data_DivisionRing
      , _Data_Semiring
      , _Prelude
      , _Control_Monad
      , _Control_Category
      , _Control_Apply
      , _Control_Bind
      , _Control_Applicative
      , _Control_Semigroupoid
      ]
  , env = fold
      [ Control.Apply.env
      , Control.Bind.env
      , Data.Bounded.env
      , Data.Enum.env
      , Data.Eq.env
      , Data.EuclideanRing.env
      , Data.Functor.env
      , Data.HeytingAlgebra.env
      , Data.Ord.env
      , Data.Ring.env
      , Data.Semigroup.env
      , Data.Semiring.env
      , Data.Show.env
      , Data.Show.Generic.env
      , Data.Unit.env
      , Data.Symbol.env
      , Record.Unsafe.env
      ]
  }

profunctor :: Typeable ctx => CoreBuild ctx
profunctor = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Profunctor
      , _Data_Profunctor_Join
      , _Data_Profunctor_Split
      , _Data_Profunctor_Star
      , _Data_Profunctor_Costrong
      , _Data_Profunctor_Strong
      , _Data_Profunctor_Choice
      , _Data_Profunctor_Cochoice
      , _Data_Profunctor_Closed
      ]
  , env = mempty
  }

psciSupport :: Typeable ctx => CoreBuild ctx
psciSupport = CoreBuild
  { buildInputs = HashMap.fromList
      [ _PSCI_Support
      ]
  , env = mempty
  }

quickcheck :: Typeable ctx => CoreBuild ctx
quickcheck = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Test_QuickCheck
      , _Test_QuickCheck_Gen
      , _Test_QuickCheck_Arbitrary
      ]
  , env = mempty
  }

random :: Typeable ctx => CoreBuild ctx
random = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Effect_Random
      ]
  , env = Effect.Random.env
  }

record :: Typeable ctx => CoreBuild ctx
record = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Record_Builder
      , _Record_Unsafe_Union
      , _Record
      ]
  , env = fold
      [ Record.Builder.env
      , Record.Unsafe.Union.env
      ]
  }

refs :: Typeable ctx => CoreBuild ctx
refs = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Effect_Ref
      ]
  , env = Effect.Ref.env
  }

safeCoerce :: Typeable ctx => CoreBuild ctx
safeCoerce = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Safe_Coerce
      ]
  , env = mempty
  }

semirings :: Typeable ctx => CoreBuild ctx
semirings = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Semiring_Free
      ]
  , env = mempty
  }

st :: Typeable ctx => CoreBuild ctx
st = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Control_Monad_ST_Ref
      , _Control_Monad_ST_Internal
      , _Control_Monad_ST_Global
      , _Control_Monad_ST_Class
      , _Control_Monad_ST
      ]
  , env = Control.Monad.ST.Internal.env
  }

strings :: Typeable ctx => CoreBuild ctx
strings = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Char_Gen
      , _Data_String
      , _Data_Char
      , _Data_String_Regex
      , _Data_String_NonEmpty_CodePoints
      , _Data_String_NonEmpty_Internal
      , _Data_String_NonEmpty_CodeUnits
      , _Data_String_NonEmpty_CaseInsensitive
      , _Data_String_CodePoints
      , _Data_String_NonEmpty
      , _Data_String_Gen
      , _Data_String_CodeUnits
      , _Data_String_Pattern
      , _Data_String_Regex_Flags
      , _Data_String_Regex_Unsafe
      , _Data_String_Common
      , _Data_String_Unsafe
      , _Data_String_CaseInsensitive
      ]
  , env = fold
      [ Data.String.CodePoints.env
      , Data.String.CodeUnits.env
      , Data.String.Common.env
      , Data.String.Regex.env
      , Data.String.Unsafe.env
      ]
  }

tailrec :: Typeable ctx => CoreBuild ctx
tailrec = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Control_Monad_Rec_Class
      ]
  , env = mempty
  }

transformers :: Typeable ctx => CoreBuild ctx
transformers = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Control_Comonad_Traced_Class
      , _Control_Comonad_Traced_Trans
      , _Control_Comonad_Env_Class
      , _Control_Comonad_Env_Trans
      , _Control_Comonad_Env
      , _Control_Comonad_Trans_Class
      , _Control_Comonad_Store
      , _Control_Comonad_Traced
      , _Control_Comonad_Store_Class
      , _Control_Comonad_Store_Trans
      , _Control_Monad_Reader_Class
      , _Control_Monad_Reader_Trans
      , _Control_Monad_Except
      , _Control_Monad_RWS_Trans
      , _Control_Monad_Identity_Trans
      , _Control_Monad_Except_Trans
      , _Control_Monad_Writer
      , _Control_Monad_Reader
      , _Control_Monad_Maybe_Trans
      , _Control_Monad_State_Class
      , _Control_Monad_State_Trans
      , _Control_Monad_Writer_Class
      , _Control_Monad_Writer_Trans
      , _Control_Monad_Cont
      , _Control_Monad_RWS
      , _Control_Monad_List_Trans
      , _Control_Monad_State
      , _Control_Monad_Trans_Class
      , _Control_Monad_Error_Class
      , _Control_Monad_Cont_Class
      , _Control_Monad_Cont_Trans
      ]
  , env = mempty
  }

tuples :: Typeable ctx => CoreBuild ctx
tuples = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Tuple
      , _Data_Tuple_Nested
  
    ]
  , env = mempty
  }

typeEquality :: Typeable ctx => CoreBuild ctx
typeEquality = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Type_Equality
      ]
  , env = mempty
  }

unfoldable :: Typeable ctx => CoreBuild ctx
unfoldable = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Unfoldable1
      , _Data_Unfoldable
      ]
  , env = fold
      [ Data.Unfoldable.env
      , Data.Unfoldable1.env
      ]
  }

unsafeCoerce :: Typeable ctx => CoreBuild ctx
unsafeCoerce = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Unsafe_Coerce
      ]
  , env = Unsafe.Coerce.env
  }

validation :: Typeable ctx => CoreBuild ctx
validation = CoreBuild
  { buildInputs = HashMap.fromList
      [ _Data_Validation_Semigroup
      , _Data_Validation_Semiring
      ]
  , env = mempty
  }