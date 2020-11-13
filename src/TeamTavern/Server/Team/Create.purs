module TeamTavern.Server.Team.Create (OkContent, BadContent, create) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Array as Array
import Data.Nullable (toNullable)
import Data.Variant (Variant, match)
import Effect (Effect)
import Perun.Request.Body (Body)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, ok_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (Handle, generateHandle)
import TeamTavern.Server.Team.Infrastructure.LogError (teamHandler)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (Team, TeamError, TeamErrors, validateTeam)

queryString :: Query
queryString = Query """
    with similar_handle_count as (
        select count(*)
        from team
        where team.handle ilike ($2 || '%')
    ),
    unique_handle as (
        select $2 || (
            case
                when (select count from similar_handle_count) = 0
                then ''
                else '' || ((select count from similar_handle_count) + 1)
            end
        ) as handle
    )
    insert into team
        ( owner_id
        , handle
        , name
        , website
        , age_from
        , age_to
        , locations
        , languages
        , microphone
        , discord_server
        , timezone
        , weekday_from
        , weekday_to
        , weekend_from
        , weekend_to
        , about
        )
    values
        ( $1, (select handle from unique_handle)
        , $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16
        )
    returning team.handle;
    """

queryParameters :: Id -> Handle -> Team -> Array QueryParameter
queryParameters ownerId handle team
    = ownerId
    : handle
    : team.name
    : toNullable team.website
    : nullableAgeFrom team.ageSpan
    : nullableAgeTo team.ageSpan
    : team.locations
    : team.languages
    : team.microphone
    : toNullable team.discordServer
    : toNullable team.timezone
    : nullableTimeFrom team.onlineWeekday
    : nullableTimeTo team.onlineWeekday
    : nullableTimeFrom team.onlineWeekend
    : nullableTimeTo team.onlineWeekend
    :| team.about

addTeam :: forall querier errors. Querier querier =>
    querier -> Id -> Handle -> Team -> Async (InternalError errors) { handle :: String }
addTeam pool ownerId handle team =
    queryFirstInternal pool queryString (queryParameters ownerId handle team)

type CreateError = Variant (internal :: Array String, client :: Array String, team :: TeamErrors)

logError :: CreateError -> Effect Unit
logError = Log.logError "Error creating team" (internalHandler >>> clientHandler >>> teamHandler)

type OkContent = { handle :: String }

type BadContent = Array TeamError

sendResponse :: Async CreateError { handle :: String } -> (forall voidLeft. Async voidLeft Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , client: const badRequest__
        , team: badRequest_ <<< writeJSON <<< Array.fromFoldable
        }
    )
    (ok_ <<< writeJSON)

create :: forall left. Pool -> Body -> Cookies -> Async left Response
create pool body cookies =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedIn pool cookies
    content <- readJsonBody body
    team <- validateTeam content
    let generatedHandle = generateHandle team.name
    addTeam pool cookieInfo.id generatedHandle team
