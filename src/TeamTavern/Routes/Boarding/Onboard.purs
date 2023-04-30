module TeamTavern.Routes.Boarding.Onboard where

import TeamTavern.Routes.Shared.TeamContacts

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Literal, NotAuthorized_, OkJson, PostJson_, Internal_)
import TeamTavern.Routes.Shared.Field (ValuesSimple, ValuesSimpleMulti)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileError)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamBase (TeamError)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileError)
import Type.Function (type ($))

type Onboard =
    PostJson_ (Literal "onboarding") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

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
    , fieldValues :: ValuesSimple
    , about :: String
    , ambitions :: String
    , newOrReturning :: Boolean
    }

type TeamProfileRequestContent =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: ValuesSimpleMulti
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
    ( team :: NonEmptyArray TeamError
    , playerProfile :: NonEmptyArray PlayerProfileError
    , teamProfile :: NonEmptyArray TeamProfileError
    , playerContacts :: NonEmptyArray PlayerContactsError
    , teamContacts :: NonEmptyArray TeamContactsError
    , other :: {}
    )
