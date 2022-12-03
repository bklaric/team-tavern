module TeamTavern.Routes.Player.DeletePlayer where

import Jarilo.Types (Delete)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (NoContent)
import Jarilo.Types (FullRoute)

type DeletePlayer = FullRoute
    Delete
    (  Literal "players"
    :> Capture "nickname" String)
    NoQuery
    NoContent
