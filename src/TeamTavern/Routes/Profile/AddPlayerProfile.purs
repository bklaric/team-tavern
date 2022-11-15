module TeamTavern.Routes.Profile.AddPlayerProfile where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)
import Type.Function (type ($))

type AddPlayerProfile = FullRoute
    (Post RequestContent)
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle)
    NoQuery
    (NoContent :! BadRequest BadContent)

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
