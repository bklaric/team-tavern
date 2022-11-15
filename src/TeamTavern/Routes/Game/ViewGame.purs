module TeamTavern.Routes.Game.ViewGame where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (Ok)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.Platform (Platforms)

type ViewGame = FullRoute
    Get
    (  Literal "games"
    :> Capture "handle" String)
    NoQuery
    (Ok OkContent)

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
