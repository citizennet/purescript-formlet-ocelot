{-
TODO: documentation
-}
let
  name = "formlet-ocelot"
in
  { name
  , dependencies =
      [ "aff"
      , "arrays"
      , "avar"
      , "bifunctors"
      , "console"
      , "const"
      , "datetime"
      , "dom-indexed"
      , "either"
      , "enums"
      , "filepond-halogen"
      , "filterable"
      , "foldable-traversable"
      , "foreign-object"
      , "formlet"
      , "formlet-halogen"
      , "fuzzy"
      , "gen"
      , "halogen"
      , "halogen-subscriptions"
      , "halogen-test-driver"
      , "identity"
      , "maybe"
      , "media-types"
      , "ocelot"
      , "option"
      , "ordered-collections"
      , "parsing"
      , "partial"
      , "pre"
      , "prelude"
      , "profunctor-lenses"
      , "quickcheck"
      , "remotedata"
      , "strings"
      , "test-unit"
      , "these"
      , "timezone"
      , "tuples"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "uri"
      , "validation"
      , "variant"
      , "web-file"
      ]
  , packages = ../../packages.dhall
  -- Due to a spago bug (see https://github.com/purescript/spago/issues/648)
  -- `sources` are relative to root instead of config file.
  , sources = [ "lib/${name}/src/**/*.purs", "lib/${name}/test/**/*.purs" ]
  }
