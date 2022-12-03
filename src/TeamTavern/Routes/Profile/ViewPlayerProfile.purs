module TeamTavern.Routes.Profile.ViewPlayerProfile where

import Jarilo (type (/), type (==>), Capture, Get, Literal, Mandatory, OkJson)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow)
import TeamTavern.Routes.Shared.PlayerBase (PlayerBaseRow)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsRow)
import TeamTavern.Routes.Shared.PlayerDetails (PlayerDetailsRow)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileRow)
import Type.Row (type (+))

type ViewPlayerProfile =
    Get
    ( Literal "players"
    / Capture "nickname" String
    / Literal "profiles"
    / Capture "handle" String)
    (Mandatory "timezone" String)
    ==> OkJson OkContent

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
