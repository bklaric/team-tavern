module TeamTavern.Routes.Profile.ViewTeamProfile where

import Jarilo.Types (Get)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (Mandatory)
import Jarilo.Types (Ok)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Shared.GameBase (GameBaseRow')
import TeamTavern.Routes.Shared.TeamBase (TeamBaseRow)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsRow)
import TeamTavern.Routes.Shared.TeamDetails (TeamDetailsRow)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileRow)
import Type.Row (type (+))

type ViewTeamProfile = FullRoute
    Get
    (  Literal "teams"
    :> Capture "teamHandle" String
    :> Literal "profiles"
    :> Capture "gameHandle" String)
    (Mandatory "timezone" String)
    (Ok OkContent)

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
