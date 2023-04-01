module TeamTavern.Routes.Player.ViewPlayer where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Get, Literal, Mandatory, NotFound_, OkJson, Internal_)
import TeamTavern.Routes.Shared.Field (Fields, ValuesSimple)
import TeamTavern.Routes.Shared.Organization (OrganizationN)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)

type ViewPlayer =
    Get
    ( Literal "players"
    / Capture "nickname" String)
    (Mandatory "timezone" String)
    ==> OkJson OkContent ! NotFound_ ! Internal_

type RouteParams =
    { nickname :: String
    , timezone :: String
    }

type OkContentProfile =
    { handle :: String
    , title :: String
    , platforms :: Platforms
    , fields :: Fields
    , platform :: Platform
    , fieldValues :: ValuesSimple
    , newOrReturning :: Boolean
    , about :: Array String
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type OkContentTeam =
    { handle :: String
    , organization :: OrganizationN
    , updated :: String
    , updatedSeconds :: Number
    }

type OkContent = PlayerContactsOpen
    ( email :: Maybe String
    , hasPassword :: Maybe Boolean
    , nickname :: String
    , birthday :: Maybe String
    , age :: Maybe Int
    , location :: Maybe String
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
    , teams :: Array OkContentTeam
    )
