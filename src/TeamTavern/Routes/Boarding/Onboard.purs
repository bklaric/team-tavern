module TeamTavern.Routes.Boarding.Onboard where

import TeamTavern.Routes.Shared.TeamContacts

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Ok)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.Size (Size)
import Type.Function (type ($))

type Onboard = FullRoute
    (Post RequestContent)
    (  Literal "onboarding")
    NoQuery
    (Ok OkContent :! BadRequest BadContent)

type PlayerRequestContent =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

type TeamRequestContent =
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

type PlayerProfileRequestContent =
    { platform :: Platform
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , about :: String
    , ambitions :: String
    , newOrReturning :: Boolean
    }

type TeamProfileRequestContent =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { ilk :: Int
    , player :: Maybe PlayerRequestContent
    , team :: Maybe TeamRequestContent
    , gameHandle :: String
    , playerProfile :: Maybe PlayerProfileRequestContent
    , teamProfile :: Maybe TeamProfileRequestContent
    , playerContacts :: Maybe PlayerContacts
    , teamContacts :: Maybe TeamContacts
    }

type OkContent = { teamHandle :: Maybe String }

type BadContent = Array $ Variant
    ( team :: Array $ Variant
        ( name :: Array String
        , website :: Array String
        )
    , playerProfile :: Array $ Variant
        ( url :: { message :: Array String, key :: String }
        , about :: Array String
        , ambitions :: Array String
        )
    , teamProfile :: Array $ Variant
        ( platforms :: Array String
        , about :: Array String
        , ambitions :: Array String
        )
    , playerContacts :: Array PlayerContactsError
    , teamContacts :: Array TeamContactsError
    )
