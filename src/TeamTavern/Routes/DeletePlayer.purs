module TeamTavern.Routes.DeletePlayer where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)

type DeletePlayer = FullRoute
    Delete
    (  Literal "players"
    :> Capture "nickname" String
    :> End)
    NoQuery
