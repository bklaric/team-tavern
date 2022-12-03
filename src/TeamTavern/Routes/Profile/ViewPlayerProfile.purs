module TeamTavern.Routes.Profile.ViewPlayerProfile where

import Jarilo.Types (Get)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (Mandatory)
import Jarilo.Types (Ok)
import Jarilo.Types (FullRoute)
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
