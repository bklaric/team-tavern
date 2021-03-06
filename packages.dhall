{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ],
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

let overrides =
    { stringutils =
        { dependencies =
            [ "arrays"
            , "either"
            , "integers"
            , "maybe"
            , "partial"
            , "prelude"
            , "strings"
            ]
        , repo = "https://github.com/menelaos/purescript-stringutils.git"
        , version = "v0.0.10"
        }
    }

let additions =
    { jarilo =
        { dependencies = [ "http-methods", "uri", "variant", "record" ]
        , repo = "https://github.com/bklaric/purescript-jarilo.git"
        , version = "v0.5.3"
        }
    , undefined =
        { dependencies = [] : List Text
        , repo = "https://github.com/bklaric/purescript-undefined.git"
        , version = "v1.0.2"
        }
    , error =
        { dependencies = [] : List Text
        , repo = "https://github.com/bklaric/purescript-error.git"
        , version = "v1.0.2"
        }
    , idiomatic-node-errors =
        { dependencies = [ "effect", "error" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-errors.git"
        , version = "v0.3.1"
        }
    , idiomatic-node-buffer =
        { dependencies = [ "foreign", "undefined" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-buffer.git"
        , version = "v0.4.1"
        }
    , idiomatic-node-events =
        { dependencies = [ "foreign", "undefined", "effect" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-events.git"
        , version = "v0.4.1"
        }
    , idiomatic-node-stream =
        { dependencies = [ "idiomatic-node-events", "idiomatic-node-buffer", "idiomatic-node-errors", "nullable", "refs" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-stream.git"
        , version = "v0.6.1"
        }
    , idiomatic-node-server =
        { dependencies = [ "idiomatic-node-events", "nullable" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-server.git"
        , version = "v0.5.1"
        }
    , idiomatic-node-http =
        { dependencies = [ "idiomatic-node-server", "idiomatic-node-stream", "foreign-object" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-http.git"
        , version = "v0.4.1"
        }
    , idiomatic-node-crypto =
        { dependencies = [ "idiomatic-node-buffer", "idiomatic-node-errors" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-crypto.git"
        , version = "v0.2.1"
        }
    , idiomatic-node-process =
        { dependencies = [ "effect", "maybe", "nullable" ]
        , repo = "https://github.com/bklaric/purescript-idiomatic-node-process.git"
        , version = "v0.3.1"
        }
    , pg =
        { dependencies = [ "idiomatic-node-events", "idiomatic-node-errors", "options", "nullable", "foreign-generic" ]
        , repo = "https://github.com/bklaric/purescript-pg.git"
        , version = "v0.5.0"
        }
    }

in  upstream // overrides // additions
