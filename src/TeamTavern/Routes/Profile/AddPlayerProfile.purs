module TeamTavern.Routes.Profile.AddPlayerProfile where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Internal_, Literal, NoContent, NotAuthorized_, PostJson_, Forbidden_)
import TeamTavern.Routes.Shared.Field (ValuesSimple)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileError)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)
import Type.Function (type ($))

type AddPlayerProfile =
    PostJson_
    ( Literal "players"
    / Capture "nickname" Nickname
    / Literal "profiles"
    / Capture "handle" Handle)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_


type RouteParams = { nickname :: Nickname, handle :: Handle }

type RequestContentProfile =
    { platform :: Platform
    , fieldValues :: ValuesSimple
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { details :: RequestContentProfile
    , contacts :: PlayerContacts
    }

type BadContent = NonEmptyArray $ Variant
    ( profile :: NonEmptyArray PlayerProfileError
    , contacts :: NonEmptyArray PlayerContactsError
    )
