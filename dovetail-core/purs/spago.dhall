{ name = "dovetail-repl"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "catenable-lists"
  , "console"
  , "const"
  , "contravariant"
  , "control"
  -- , "datetime"
  , "distributive"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  -- , "foreign"
  -- , "foreign-object"
  , "free"
  , "functions"
  , "functors"
  , "gen"
  , "graphs"
  , "identity"
  , "integers"
  , "invariant"
  , "lazy"
  , "lcg"
  , "lists"
  , "math"
  , "maybe"
  , "minibench"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "orders"
  , "parallel"
  , "partial"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "record"
  , "refs"
  , "safe-coerce"
  , "semirings"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
