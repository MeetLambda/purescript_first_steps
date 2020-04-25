{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "tuples"
  , "unicode"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
