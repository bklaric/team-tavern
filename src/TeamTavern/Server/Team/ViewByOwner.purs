module TeamTavern.Server.Team.ViewByOwner (Team, viewByOwner) where

import Prelude

import Async (alwaysRight, examineLeftWithEffect)
import Perun.Response (internalServerError__, ok_)
import Postgres.Query (Query(..), (:))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (internalHandler, queryMany)

type Team = { name :: String, handle :: String }

queryString :: Query
queryString = Query """
    select team.name, team.handle
    from team
        join player on player.id = team.owner_id
    where lower(player.nickname) = lower($1);
    """

loadTeams pool nickname = queryMany pool queryString (nickname : [])

logError = Log.logError "Error viewing teams by owner" internalHandler

sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

viewByOwner pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    -- Load teams from database by owner and just send them lol.
    teams :: Array Team <- loadTeams pool routeParams.nickname
    pure teams
