module TeamTavern.Routes.ViewCompetitions where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)

type ViewCompetitions = FullRoute
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "competitions"
    :> End)
    NoQuery

type OkCompetitionRow fields =
    ( handle :: String
    , name :: String
    , description :: Array String
    , website :: Maybe String
    , discordServer :: Maybe String
    , region :: String
    , signupDeadlineSeconds :: Maybe Number
    | fields
    )

type OkCompetitionContent = Record (OkCompetitionRow ())

type OkContent = Array OkCompetitionContent
