module TeamTavern.Server.Team.Update (OkContent, BadContent, update) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Array as Array
import Data.Nullable (toNullable)
import Data.Variant (Variant, match)
import Effect (Effect)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_, unauthorized__)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, notAuthenticatedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Team.Infrastructure.LogError (teamHandler)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (Team, TeamError, TeamErrors, organizationName, organizationWebsite, toString, validateTeam)

queryString :: Query
queryString = Query """
    update team
    set
        organization = $3,
        name = $4,
        website = $5,
        discord_tag = $6,
        discord_server = $7,
        age_from = $8,
        age_to = $9,
        locations = $10,
        languages = $11,
        microphone = $12,
        timezone = $13,
        weekday_from = $14,
        weekday_to = $15,
        weekend_from = $16,
        weekend_to = $17,
        about = $18,
        updated = now()
    where owner_id = $1 and handle = $2
    """

queryParameters :: Id -> String -> Team -> Array QueryParameter
queryParameters ownerId handle team
    = ownerId
    : handle
    : (toString team.organization)
    : (toNullable $ organizationName team.organization)
    : (toNullable $ organizationWebsite team.organization)
    : toNullable team.discordTag
    : toNullable team.discordServer
    : nullableAgeFrom team.ageSpan
    : nullableAgeTo team.ageSpan
    : team.locations
    : team.languages
    : team.microphone
    : toNullable team.timezone
    : nullableTimeFrom team.onlineWeekday
    : nullableTimeTo team.onlineWeekday
    : nullableTimeFrom team.onlineWeekend
    : nullableTimeTo team.onlineWeekend
    :| team.about

updateTeam :: forall querier errors. Querier querier =>
    querier -> Id -> String -> Team -> Async (InternalError errors) Unit
updateTeam pool ownerId handle team =
    queryNone pool queryString (queryParameters ownerId handle team)

type CreateError = Variant
    ( internal :: Array String
    , notAuthenticated :: Array String
    , client :: Array String
    , team :: TeamErrors
    )

logError :: CreateError -> Effect Unit
logError = Log.logError "Error updating team"
    (internalHandler >>> notAuthenticatedHandler >>> clientHandler >>> teamHandler)

type OkContent = { handle :: String }

type BadContent = Array TeamError

sendResponse :: Async CreateError Unit -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , notAuthenticated: const unauthorized__
        , client: const badRequest__
        , team: badRequest_ <<< writeJSON <<< Array.fromFoldable
        }
    )
    (const $ noContent_)

update :: forall left. Pool -> Body -> Cookies -> { handle :: String } -> Async left Response
update pool body cookies { handle } =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedIn pool cookies
    content <- readJsonBody body
    team <- validateTeam content
    updateTeam pool cookieInfo.id handle team
