module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)

type StartSession = FullRoute
    (Post RequestContent)
    (Literal "sessions")
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent =
    { nickname :: String
    , password :: String
    }

type BadContent = Variant (noSessionStarted :: {})
