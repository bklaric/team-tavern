module TeamTavern.Routes.ViewCompetitions where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewCompetitions = Route
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "competitions"
    :> End)
    NoQuery

type OkCompetitionContent =
    { name :: String
    , description :: Array String
    , website :: Maybe String
    , discordServer :: Maybe String
    }

type OkContent = Array OkCompetitionContent
