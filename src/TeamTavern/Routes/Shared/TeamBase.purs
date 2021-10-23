module TeamTavern.Routes.Shared.TeamBase where

type TeamBaseRow fields =
    ( owner :: String
    , handle :: String
    | fields
    )

type TeamBaseOpen fields = Record (TeamBaseRow fields)

type TeamBase = TeamBaseOpen ()
