module TeamTavern.Routes.Player.UpdatePlayer where


import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, PutJson_)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type UpdatePlayer =
    PutJson_ (Literal "players" / Capture "nickname" Nickname) RequestContent
    ==> NoContent ! NotAuthorized_ ! Forbidden_ ! Internal_

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
