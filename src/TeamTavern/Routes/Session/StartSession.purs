module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Forbidden_, Internal_, NoContent)
import Jarilo.Types (FullRoute)

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
