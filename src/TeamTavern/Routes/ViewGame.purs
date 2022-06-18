module TeamTavern.Routes.ViewGame where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Platform (Platforms)

type ViewGame = FullRoute
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> End)
    NoQuery

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
