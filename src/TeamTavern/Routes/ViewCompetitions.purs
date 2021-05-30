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

type OkCompetitionRow fields =
    ( name :: String
    , description :: Array String
    , banner :: String
    , website :: Maybe String
    , discordServer :: Maybe String
    , region :: String
    | fields
    )

type OkCompetitionContent = Record (OkCompetitionRow ())

type OkContent = Array OkCompetitionContent