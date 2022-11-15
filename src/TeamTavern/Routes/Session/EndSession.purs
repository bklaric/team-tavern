module TeamTavern.Routes.Session.EndSession where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (NoContent)
import Jarilo.Route (FullRoute)

type EndSession = FullRoute
    Delete
    (  Literal "sessions"
    :> Literal "current")
    NoQuery
    NoContent
