module TeamTavern.Routes.Session.EndSession where

import Jarilo.Types (Delete)
import Jarilo.Types (type (:>), Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (NoContent)
import Jarilo.Types (FullRoute)

type EndSession = FullRoute
    Delete
    (  Literal "sessions"
    :> Literal "current")
    NoQuery
    NoContent
