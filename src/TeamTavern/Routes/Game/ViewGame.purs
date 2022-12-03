module TeamTavern.Routes.Game.ViewGame where

import Data.Maybe (Maybe)
import Jarilo.Types (Get)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (Ok)
import Jarilo.Types (FullRoute)
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
