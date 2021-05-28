module TeamTavern.Routes.ViewAllGames where

import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type ViewAllGames = Route
    Get
    (  Literal "games"
    :> End)
    NoQuery

type OkGameContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , description :: Array String
    }

type OkContent = Array OkGameContent
