module TeamTavern.Routes.DeleteAlert where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)

type DeleteAlert = FullRoute
    Delete
    (  Literal "alerts"
    :> Capture "id" Int
    :> End)
    (Mandatory "token" String)

type RouteContent = { id :: Int, token :: String }
