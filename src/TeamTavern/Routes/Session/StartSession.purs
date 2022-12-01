module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, Forbidden_, Internal_, NoContent)
import Jarilo.Route (FullRoute)

type StartSession = FullRoute
    (Post RequestContent)
    (Literal "sessions")
    NoQuery
    (NoContent :! BadRequest BadContent :! Forbidden_ :! Internal_)

type RequestContent =
    { nickname :: String
    , password :: String
    }

type BadContent = Variant (noSessionStarted :: {})
