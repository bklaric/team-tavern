module TeamTavern.Routes.Game.ViewAllGames where

import Jarilo (type (==>), Get_, Literal, OkJson)

type ViewAllGames =
    Get_ (Literal "games")
    ==> OkJson OkContent

type OkGameContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , description :: Array String
    }

type OkContent = Array OkGameContent
