module TeamTavern.Server.Team.ViewByOwner (Team, viewByOwner) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Effect (Effect)
import Perun.Response (Response, internalServerError__, ok_)
import Postgres.Query (class Querier, Query(..), (:))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (logInternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

-- TODO: Obrisati sve vezano za ViewByOwner

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

loadTeams :: forall errors querier. Querier querier =>
    querier -> String -> Async (InternalError errors) (Array Team)
loadTeams pool nickname = queryMany pool queryString (nickname : [])

logError :: InternalError () -> Effect Unit
logError = logInternalError "Error viewing teams by owner"

sendResponse :: forall errors.
    Async (InternalError errors) (Array Team) -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

viewByOwner :: forall left querier. Querier querier =>
    querier -> { nickname :: String } -> Async left Response
viewByOwner pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    -- Load teams from database by owner and just send them lol.
    teams :: Array Team <- loadTeams pool routeParams.nickname
    pure teams
