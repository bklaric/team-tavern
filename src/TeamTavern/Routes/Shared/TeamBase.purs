module TeamTavern.Routes.Shared.TeamBase where

import Data.Variant (Variant)

type TeamBaseRow fields =
    ( owner :: String
    , handle :: String
    | fields
    )

type TeamBaseOpen fields = Record (TeamBaseRow fields)

type TeamBase = TeamBaseOpen ()

type TeamError = Variant
    ( name :: {}
    , website :: {}
    )
