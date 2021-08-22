module TeamTavern.Routes.Onboard where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
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
    }

type TeamRequestContent =
    { organization :: OrganizationNW
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
    }

type PlayerProfileRequestContent =
    { platform :: Platform
    , platformId :: String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , about :: String
    , newOrReturning :: Boolean
    }

type TeamProfileRequestContent =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , newOrReturning :: Boolean
    , about :: String
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
        )
    , team :: Array $ Variant
        ( name :: Array String
        , website :: Array String
        , discordTag :: Array String
        , discordServer :: Array String
        , contact :: Array String
        )
    , playerProfile :: Array $ Variant
        ( platformId :: Array String
        , url :: { message :: Array String, key :: String }
        , about :: Array String
        )
    , teamProfile :: Array $ Variant
        ( platforms :: Array String
        , about :: Array String
        )
    )
