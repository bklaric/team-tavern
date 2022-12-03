module TeamTavern.Routes.Team.CreateTeam where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Ok)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)

type CreateTeam = FullRoute
    (Post RequestContent)
    (Literal "teams")
    NoQuery
    (Ok OkContent :! BadRequest BadContent)

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
