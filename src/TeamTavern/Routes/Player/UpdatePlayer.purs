module TeamTavern.Routes.Player.UpdatePlayer where


import Data.Maybe (Maybe)
import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (NoContent)
import Jarilo.Types (FullRoute)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type UpdatePlayer = FullRoute
    (Put RequestContent)
    (  Literal "players"
    :> Capture "nickname" Nickname)
    NoQuery
    NoContent

type RequestContent =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }
