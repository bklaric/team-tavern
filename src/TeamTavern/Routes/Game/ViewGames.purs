module TeamTavern.Routes.Game.ViewGames where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, OkJson)

type ViewGames =
    Get_ (Literal "games")
    ==> OkJson OkContent ! Internal_

type OkGameContent =
    { handle :: String
    , title :: String
    }

type OkContent = Array OkGameContent
