module TeamTavern.Routes.Game.ViewAllGames where

import Jarilo (type (!), type (==>), Get_, Literal, OkJson, Internal_)

type ViewAllGames =
    Get_ (Literal "games")
    ==> OkJson OkContent ! Internal_

type OkGameContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , description :: Array String
    }

type OkContent = Array OkGameContent
