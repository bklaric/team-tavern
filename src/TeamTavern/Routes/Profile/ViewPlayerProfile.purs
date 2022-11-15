module TeamTavern.Routes.Profile.ViewPlayerProfile where

import Jarilo.Method (Get)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (Mandatory)
import Jarilo.Response (Ok)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow)
import TeamTavern.Routes.Shared.PlayerBase (PlayerBaseRow)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsRow)
import TeamTavern.Routes.Shared.PlayerDetails (PlayerDetailsRow)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileRow)
import Type.Row (type (+))

type ViewPlayerProfile = FullRoute
    Get
    (  Literal "players"
    :> Capture "nickname" String
    :> Literal "profiles"
    :> Capture "handle" String)
    (Mandatory "timezone" String)
    (Ok OkContent)

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
