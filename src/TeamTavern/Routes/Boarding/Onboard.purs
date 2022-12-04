module TeamTavern.Routes.Boarding.Onboard where

import TeamTavern.Routes.Shared.TeamContacts

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), Literal, OkJson, PostJson_, BadRequestJson)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.Size (Size)
import Type.Function (type ($))

type Onboard =
    PostJson_ (Literal "onboarding") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent

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

type BadContent = NonEmptyArray $ Variant
    ( team :: NonEmptyArray $ Variant
        ( name :: {}
        , website :: {}
        )
    , playerProfile :: NonEmptyArray $ Variant
        ( url :: { key :: String }
        , about :: {}
        , ambitions :: {}
        )
    , teamProfile :: NonEmptyArray $ Variant
        ( platforms :: {}
        , about :: {}
        , ambitions :: {}
        )
    , playerContacts :: NonEmptyArray PlayerContactsError
    , teamContacts :: NonEmptyArray TeamContactsError
    )
