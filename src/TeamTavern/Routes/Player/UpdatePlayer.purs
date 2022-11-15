module TeamTavern.Routes.Player.UpdatePlayer where


import Data.Maybe (Maybe)
import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (NoContent)
import Jarilo.Route (FullRoute)
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
