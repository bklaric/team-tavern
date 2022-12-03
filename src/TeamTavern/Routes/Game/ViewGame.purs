module TeamTavern.Routes.Game.ViewGame where

import Data.Maybe (Maybe)
import Jarilo (type (/), type (==>), Capture, Get_, Literal, OkJson)
import TeamTavern.Routes.Shared.Platform (Platforms)

type ViewGame =
    Get_ (Literal "games" / Capture "handle" String)
    ==> OkJson OkContent

type OkContentOption =
    { key :: String
    , label :: String
    }

type OkContentField =
    { ilk :: Int
    , label :: String
    , key :: String
    , icon :: String
    , domain :: Maybe String
    , options :: Maybe (Array OkContentOption)
    }

type OkContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , platforms :: Platforms
    , fields :: Array OkContentField
    }
