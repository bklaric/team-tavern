module TeamTavern.Server.Team.ViewByOwner (Team, viewByOwner) where

import Prelude

import Async (alwaysRight, examineLeftWithEffect)
import Perun.Response (internalServerError__, ok_)
import Postgres.Query (Query(..), (:))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Log (logInternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

type Team =
    { name :: String
    , handle :: String
    , updated :: String
    , updatedSeconds :: Number
    }

queryString :: Query
queryString = Query """
    select
        team.name,
        team.handle,
        team.updated::text,
        extract(epoch from (now() - team.updated)) as "updatedSeconds"
    from team
        join player on player.id = team.owner_id
    where lower(player.nickname) = lower($1)
    order by updated desc
    """

loadTeams pool nickname = queryMany pool queryString (nickname : [])

logError = logInternalError "Error viewing teams by owner"

sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

viewByOwner pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    -- Load teams from database by owner and just send them lol.
    teams :: Array Team <- loadTeams pool routeParams.nickname
    pure teams
