module TeamTavern.Routes.Onboarding where

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import Type (type ($))

type Onboard = Route
    Post
    (  Literal "onboarding"
    :> End)
    NoQuery

type RequestContent =
    { ilk :: Int
    , player :: Maybe
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
    , team :: Maybe
        { name :: String
        , website :: Maybe String
        , ageFrom :: Maybe Int
        , ageTo :: Maybe Int
        , locations :: Array String
        , languages :: Array String
        , microphone :: Boolean
        , discordServer :: Maybe String
        , timezone :: Maybe String
        , weekdayFrom :: Maybe String
        , weekdayTo :: Maybe String
        , weekendFrom :: Maybe String
        , weekendTo :: Maybe String
        , about :: String
        }
    , gameHandle :: String
    , playerProfile :: Maybe
        { fieldValues :: Array
            { fieldKey :: String
            , url :: Maybe String
            , optionKey :: Maybe String
            , optionKeys :: Maybe (Array String)
            }
        , ambitions :: String
        , newOrReturning :: Boolean
        }

    , teamProfile :: Maybe
        { fieldValues :: Array
            { fieldKey :: String
            , optionKeys :: Array String
            }
        , newOrReturning :: Boolean
        , ambitions :: String
        }
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
    )
