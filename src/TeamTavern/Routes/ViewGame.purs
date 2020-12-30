module TeamTavern.Routes.ViewGame where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewGame = Route
    Get
    (  Literal "games"
    :> Literal "by-handle"
    :> Capture "handle" String
    :> End)
    NoQuery

type OkContent =
    { title :: String
    , handle :: String
    , externalIdIlk :: Int
    , fields :: Array
        { ilk :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    }
