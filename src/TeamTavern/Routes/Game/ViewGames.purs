module TeamTavern.Routes.Game.ViewGames where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, OkJson)

type ViewGames =
    Get_ (Literal "games")
    ==> OkJson OkContent ! Internal_

-- | `active` counts the game's posts that haven't expired.
type OkGameContent =
    { handle :: String
    , title :: String
    , active :: Int
    }

type OkContent = Array OkGameContent
