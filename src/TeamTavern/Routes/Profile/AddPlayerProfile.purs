module TeamTavern.Routes.Profile.AddPlayerProfile where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, NoContent, PostJson_)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)
import Type.Function (type ($))

type AddPlayerProfile =
    PostJson_
    ( Literal "players"
    / Capture "nickname" Nickname
    / Literal "profiles"
    / Capture "handle" Handle)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent

type RouteParams = { nickname :: Nickname, handle :: Handle }

type RequestContentFieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type RequestContentProfile =
    { platform :: Platform
    , fieldValues :: Array RequestContentFieldValue
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { details :: RequestContentProfile
    , contacts :: PlayerContacts
    }

type BadContent = Array $ Variant
    ( profile :: Array $ Variant
        ( about :: Array String
        , ambitions :: Array String
        , url :: { key :: String, message :: Array String }
        )
    , contacts :: Array PlayerContactsError
    )
