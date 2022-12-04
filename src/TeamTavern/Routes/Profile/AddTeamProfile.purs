module TeamTavern.Routes.Profile.AddTeamProfile where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, NoContent, PostJson_)
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
    ==> NoContent ! BadRequestJson BadContent

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
