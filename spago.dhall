{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "team-tavern-server"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-svg-elems"
  , "heterogeneous"
  , "jarilo"
  , "js-timers"
  , "pg"
  , "psci-support"
  , "stringutils"
  , "typelevel-prelude"
  , "undefined"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
