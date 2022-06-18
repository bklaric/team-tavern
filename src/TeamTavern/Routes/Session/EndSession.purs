module TeamTavern.Routes.Session.EndSession where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)

type EndSession = FullRoute
    Delete
    (  Literal "sessions"
    :> Literal "current"
    :> End)
    NoQuery
