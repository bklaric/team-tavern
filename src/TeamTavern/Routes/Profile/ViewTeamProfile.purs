module TeamTavern.Routes.Profile.ViewTeamProfile where

import Jarilo (type (!), type (/), type (==>), Capture, Get, Internal_, Literal, Mandatory, OkJson, NotFound_)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow')
import TeamTavern.Routes.Shared.TeamBase (TeamBaseRow)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsRow)
import TeamTavern.Routes.Shared.TeamDetails (TeamDetailsRow)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileRow)
import Type.Row (type (+))

type ViewTeamProfile =
    Get
    ( Literal "teams"
    / Capture "teamHandle" String
    / Literal "profiles"
    / Capture "gameHandle" String)
    (Mandatory "timezone" String)
    ==> OkJson OkContent ! NotFound_ ! Internal_

type RouteParams =
    { teamHandle :: String
    , gameHandle :: String
    , timezone :: String
    }

type OkContent = Record
    ( TeamBaseRow
    + TeamContactsRow
    + TeamDetailsRow
    + TeamProfileRow
    + GameBaseRow'
    + ()
    )
