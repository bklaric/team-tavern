module TeamTavern.Routes.Onboarding where

import Data.Maybe (Maybe)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

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
