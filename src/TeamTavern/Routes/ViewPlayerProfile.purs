module TeamTavern.Routes.Shared.ViewPlayerProfile where

import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow)
import TeamTavern.Routes.Shared.PlayerBase (PlayerBaseRow)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsRow)
import TeamTavern.Routes.Shared.PlayerDetails (PlayerDetailsRow)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileRow)
import Type.Row (type (+))

type ViewPlayerProfile = Route
    Get
    (  Literal "players"
    :> Capture "nickname" String
    :> Literal "profiles"
    :> Capture "handle" String
    :> End)
    (Mandatory "timezone" String)

type RouteParams =
    { nickname :: String
    , handle :: String
    , timezone :: String
    }

type OkContent = Record
    ( PlayerBaseRow
    + PlayerContactsRow
    + PlayerDetailsRow
    + PlayerProfileRow
    + GameBaseRow
    + ()
    )
