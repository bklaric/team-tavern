{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "team-tavern-server"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "contravariant"
  , "control"
  , "css"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "error"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "functions"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-svg-elems"
  , "heterogeneous"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "nodey"
  , "nonempty"
  , "now"
  , "nullable"
  , "options"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "partial"
  , "pg"
  , "prelude"
  , "record"
  , "refs"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined"
  , "unfoldable"
  , "unicode"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "uri"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
