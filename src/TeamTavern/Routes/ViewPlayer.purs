module TeamTavern.Routes.ViewPlayer where

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type RouteParams =
    { nickname :: String
    , timezone :: String
    }

type OkContentProfile =
    { handle :: String
    , title :: String
    , externalIdIlk :: Int
    , fields :: Array
        { key :: String
        , ilk :: Int
        , label :: String
        , icon :: String
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , externalId :: String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , newOrReturning :: Boolean
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type OkContentTeam =
    { name :: String
    , handle :: String
    , updated :: String
    , updatedSeconds :: Number
    }

type OkContent =
    { nickname :: String
    , birthday :: Maybe String
    , age :: Maybe Int
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , weekendOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , discordTag :: Maybe String
    , steamUrl :: Maybe String
    , riotId :: Maybe String
    , about :: Array String
    , profiles :: Array OkContentProfile
    , teams :: Array OkContentTeam
    }

type ViewPlayer = Route
    Get
    (  Literal "players"
    :> Capture "nickname" String
    :> End)
    (Mandatory "timezone" String)
