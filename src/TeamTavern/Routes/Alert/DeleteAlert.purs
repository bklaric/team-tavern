module TeamTavern.Routes.Alert.DeleteAlert where

import Jarilo.Types (Delete)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (Mandatory)
import Jarilo.Types (NoContent)
import Jarilo.Types (FullRoute)

type DeleteAlert = FullRoute
    Delete
    (  Literal "alerts"
    :> Capture "id" Int)
    (Mandatory "token" String)
    NoContent

type RouteContent = { id :: Int, token :: String }
