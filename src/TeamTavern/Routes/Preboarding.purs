module TeamTavern.Routes.Preboarding where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Onboarding (PlayerProfileRequestContent, PlayerRequestContent, TeamRequestContent, TeamProfileRequestContent)
import Type (type ($))

type Preboard = Route
    Post
    (  Literal "preboarding"
    :> End)
    NoQuery

type RegisterRequestContent =
    { nickname :: String
    , email :: String
    , password :: String
    }

type RequestContent =
    { ilk :: Int
    , player :: Maybe PlayerRequestContent
    , team :: Maybe TeamRequestContent
    , gameHandle :: String
    , playerProfile :: Maybe PlayerProfileRequestContent
    , teamProfile :: Maybe TeamProfileRequestContent
    , registration :: RegisterRequestContent
    }

type OkContent = { teamHandle :: String }

type BadContent = Array $ Variant
    ( player :: Array $ Variant
        ( discordTag :: Array String
        , about :: Array String
        )
    , team :: Array $ Variant
        ( name :: Array String
        , website :: Array String
        , discordServer :: Array String
        , about :: Array String
        )
    , playerProfile :: Array $ Variant
        ( url :: { message :: Array String, key :: String }
        , missing :: { message :: Array String, key :: String }
        , ambitions :: Array String
        )
    , teamProfile :: Array $ Variant
        ( ambitions :: Array String )
    , registration :: Array $ Variant
        ( nickname :: Array String
        , email :: Array String
        , password :: Array String
        )
    , emailTaken :: Array String
    , nicknameTaken :: Array String
    )
