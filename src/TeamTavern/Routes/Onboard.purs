module TeamTavern.Routes.Onboard where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Shared.ExternalIdIlk (ExternalIdIlk)
import Type (type ($))

type Onboard = Route
    Post
    (  Literal "onboarding"
    :> End)
    NoQuery

type PlayerRequestContent =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , discordTag :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type TeamRequestContent =
    { name :: String
    , website :: Maybe String
    , discordTag :: Maybe String
    , discordServer :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type PlayerProfileRequestContent =
    { externalIdIlk :: ExternalIdIlk
    , externalId :: String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , ambitions :: String
    , newOrReturning :: Boolean
    }

type TeamProfileRequestContent =
    { fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , newOrReturning :: Boolean
    , ambitions :: String
    }

type RequestContent =
    { ilk :: Int
    , player :: Maybe PlayerRequestContent
    , team :: Maybe TeamRequestContent
    , gameHandle :: String
    , playerProfile :: Maybe PlayerProfileRequestContent
    , teamProfile :: Maybe TeamProfileRequestContent
    }

type OkContent = { teamHandle :: Maybe String }

type BadContent = Array $ Variant
    ( player :: Array $ Variant
        ( discordTag :: Array String
        , about :: Array String
        )
    , team :: Array $ Variant
        ( name :: Array String
        , website :: Array String
        , discordTag :: Array String
        , discordServer :: Array String
        , contact :: Array String
        , about :: Array String
        )
    , playerProfile :: Array $ Variant
        ( externalId :: Array String
        , url :: { message :: Array String, key :: String }
        , ambitions :: Array String
        )
    , teamProfile :: Array $ Variant
        ( ambitions :: Array String )
    )
