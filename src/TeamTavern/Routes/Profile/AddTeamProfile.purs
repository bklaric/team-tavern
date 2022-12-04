module TeamTavern.Routes.Profile.AddTeamProfile where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Literal, NoContent, NotAuthorized_, PostJson_, Internal_)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileError)
import TeamTavern.Routes.Shared.Types (Handle)
import Type.Function (type ($))

type AddTeamProfile =
    PostJson_
    ( Literal "teams"
    / Capture "teamHandle" Handle
    / Literal "profiles"
    / Capture "gameHandle" Handle)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RouteParams = { teamHandle :: Handle, gameHandle :: Handle }

type RequestContentFieldValue =
    { fieldKey :: String
    , optionKeys :: Array String
    }

type RequestContentProfile =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: Array RequestContentFieldValue
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { details :: RequestContentProfile
    , contacts :: TeamContacts
    }

type BadContent = NonEmptyArray $ Variant
    ( profile :: NonEmptyArray TeamProfileError
    , contacts :: NonEmptyArray TeamContactsError
    )
