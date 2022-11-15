module TeamTavern.Routes.Team.ViewTeam where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (Mandatory)
import Jarilo.Response (Ok)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsOpen)

type ViewTeam = FullRoute
    Get
    (  Literal "teams"
    :> Capture "handle" String)
    (Mandatory "timezone" String)
    (Ok OkContent)

type RouteParams = { handle :: String, timezone :: String }

type OkContentProfile =
    { handle :: String
    , title :: String
    , allPlatforms :: Platforms
    , size :: Size
    , selectedPlatforms :: Array Platform
    , fields :: Array
        { key :: String
        , label :: String
        , icon :: String
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , newOrReturning :: Boolean
    , about :: Array String
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type OkContent = TeamContactsOpen
    ( owner :: String
    , handle :: String
    , organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , weekendOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , profiles :: Array OkContentProfile
    )
