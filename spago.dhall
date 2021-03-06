{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "team-tavern-server"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "foreign-object"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-svg-elems"
  , "idiomatic-node-crypto"
  , "idiomatic-node-http"
  , "idiomatic-node-process"
  , "jarilo"
  , "pg"
  , "psci-support"
  , "record-extra"
  , "simple-json"
  , "stringutils"
  , "undefined"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
