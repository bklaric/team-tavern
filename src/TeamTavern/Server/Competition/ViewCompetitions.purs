module TeamTavern.Server.Competition.ViewCompetitions where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Variant (match)
import Effect (Effect)
import Perun.Response (Response, internalServerError__, ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.ViewCompetitions as ViewCompetitions
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (logInternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

queryString :: Query
queryString = Query """
    select
        competition.handle,
        competition.name,
        competition.description,
        competition.website,
        competition.discord_server as "discordServer",
        competition.region,
        case
            when extract(epoch from (signup_deadline + time '23:59' - now())) > 0
            then extract(epoch from (signup_deadline + time '23:59' - now()))
            else null
        end as "signupDeadlineSeconds"
    from competition
        join game on game.id = competition.game_id
    where game.handle = $1;
    """

loadCompetitions :: forall errors.
    Pool -> String -> Async (InternalError errors) ViewCompetitions.OkContent
loadCompetitions pool handle = queryMany pool queryString (handle : [])

logError :: InternalError () -> Effect Unit
logError = logInternalError "Error viewing competitions"

errorResponse :: InternalError () -> Response
errorResponse = match
    { internal: const $ internalServerError__
    }

successResponse :: ViewCompetitions.OkContent -> Response
successResponse result = ok_ $ writeJSON result

sendResponse ::
    Async (InternalError ()) ViewCompetitions.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse

viewCompetitions :: forall left. Pool -> String -> Async left Response
viewCompetitions pool handle =
    sendResponse $ examineLeftWithEffect logError do
    -- Load game from database.
    loadCompetitions pool handle
