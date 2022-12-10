module TeamTavern.Routes.Boarding.Preboard where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NotAuthorized_, OkJson, PostJson_, Forbidden_)
import TeamTavern.Routes.Boarding.Onboard (PlayerProfileRequestContent, PlayerRequestContent, TeamProfileRequestContent, TeamRequestContent)
import TeamTavern.Routes.Player.RegisterPlayer (RegistrationError)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsError, PlayerContacts)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileError)
import TeamTavern.Routes.Shared.TeamBase (TeamError)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsError, TeamContacts)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileError)
import Type.Function (type ($))

type Preboard =
    PostJson_ (Literal "preboarding") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RegisterRequestContent =
    { nickname :: String
    , password :: String
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
    , registration :: RegisterRequestContent
    }

type OkContent = { teamHandle :: Maybe String }

type BadContent = NonEmptyArray $ Variant
    ( team :: NonEmptyArray TeamError
    , playerProfile :: NonEmptyArray PlayerProfileError
    , teamProfile :: NonEmptyArray TeamProfileError
    , playerContacts :: NonEmptyArray PlayerContactsError
    , teamContacts :: NonEmptyArray TeamContactsError
    , registration :: NonEmptyArray RegistrationError
    , nicknameTaken :: {}
    , other :: {}
    )
