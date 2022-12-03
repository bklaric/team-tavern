module TeamTavern.Routes.Boarding.Preboard where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Ok)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Boarding.Onboard (PlayerProfileRequestContent, PlayerRequestContent, TeamProfileRequestContent, TeamRequestContent)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsError, PlayerContacts)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsError, TeamContacts)
import Type.Function (type ($))

type Preboard = FullRoute
    (Post RequestContent)
    (  Literal "preboarding")
    NoQuery
    (Ok OkContent :! BadRequest BadContent)

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
    , registration :: Array $ Variant
        ( nickname :: Array String
        , password :: Array String
        )
    , nicknameTaken :: Array String
    )
