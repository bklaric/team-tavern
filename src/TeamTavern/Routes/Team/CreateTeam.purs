module TeamTavern.Routes.Team.CreateTeam where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)

type CreateTeam = FullRoute
    Post
    (  Literal "teams"
    :> End)
    NoQuery

type RequestContent =
    { organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

type OkContent =
    { id :: Int
    , handle :: String
    }

type BadContentError = Variant
    ( name :: Array String
    , website :: Array String
    )

type BadContent = Array BadContentError
