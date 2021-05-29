module TeamTavern.Routes.ViewCompetitions where

import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewAllGames = Route
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "competitions"
    :> End)
    NoQuery

type OkCompetitionContent =
    { name :: String
    , link :: String
    , description :: Array String
    }

type OkContent = Array OkCompetitionContent
