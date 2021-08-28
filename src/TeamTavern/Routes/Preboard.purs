module TeamTavern.Routes.Preboard where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Onboard (PlayerProfileRequestContent, PlayerRequestContent, TeamProfileRequestContent, TeamRequestContent)
import TeamTavern.Routes.Shared.Player as Player
import Type (type ($))

type Preboard = Route
    Post
    (  Literal "preboarding"
    :> End)
    NoQuery

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
    , playerContacts :: Maybe Player.Contacts
    -- , teamContact :: Maybe TeamContactRequestContent
    , registration :: RegisterRequestContent
    }

type OkContent = { teamHandle :: Maybe String }

type BadContent = Array $ Variant
    ( team :: Array $ Variant
        ( name :: Array String
        , website :: Array String
        , discordTag :: Array String
        , discordServer :: Array String
        , contact :: Array String
        )
    , playerProfile :: Array $ Variant
        ( url :: { message :: Array String, key :: String }
        , about :: Array String
        )
    , teamProfile :: Array $ Variant
        ( platforms :: Array String
        , about :: Array String
        )
    , playerContacts :: Array Player.ContactsError
    , registration :: Array $ Variant
        ( nickname :: Array String
        , password :: Array String
        )
    , nicknameTaken :: Array String
    )
