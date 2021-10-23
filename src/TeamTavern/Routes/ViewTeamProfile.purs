module TeamTavern.Routes.ViewTeamProfile where

import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow')
import TeamTavern.Routes.Shared.TeamBase (TeamBaseRow)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsRow)
import TeamTavern.Routes.Shared.TeamDetails (TeamDetailsRow)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileRow)
import Type.Row (type (+))

type ViewTeamProfile = Route
    Get
    (  Literal "teams"
    :> Capture "teamHandle" String
    :> Literal "profiles"
    :> Capture "gameHandle" String
    :> End)
    (Mandatory "timezone" String)

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
