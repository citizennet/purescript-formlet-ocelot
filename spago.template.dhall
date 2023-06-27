{-
{{GENERATED_DOC}}

The `formlet-ocelot` package contains implementations for the `formlet`
abstractions, specific to the Ocelot component library.
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
  -- This path is relative to config file
  , packages = {{PACKAGES_DIR}}/packages.dhall
  -- This path is relative to project root
  -- See https://github.com/purescript/spago/issues/648
  , sources = [ "{{SOURCES_DIR}}/src/**/*.purs", "{{SOURCES_DIR}}/test/**/*.purs" ]
  }
