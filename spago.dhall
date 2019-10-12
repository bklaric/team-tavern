{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "team-tavern-server"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "psci-support"
    , "unicode"
    , "halogen"
    , "simple-json"
    , "foreign-generic"
    , "foreign-object"
    , "jarilo"
    , "undefined"
    , "idiomatic-node-crypto"
    , "idiomatic-node-http"
    , "idiomatic-node-process"
    , "pg"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
