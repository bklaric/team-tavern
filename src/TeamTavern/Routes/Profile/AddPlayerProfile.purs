module TeamTavern.Routes.Profile.AddPlayerProfile where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)
import Type.Function (type ($))

type AddPlayerProfile = FullRoute
    Post
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle
    :> End)
    NoQuery

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
