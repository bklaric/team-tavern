module TeamTavern.Server.Team.Update (update) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Routes.Team.UpdateTeam as UpdateTeam
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (Team, organizationName, organizationWebsite, toString, validateTeam)

queryString :: Query
queryString = Query """
    update team
    set
        organization = $3,
        name = $4,
        website = $5,
        age_from = $6,
        age_to = $7,
        locations = $8,
        languages = $9,
        microphone = $10,
        timezone = $11,
        weekday_from = $12,
        weekday_to = $13,
        weekend_from = $14,
        weekend_to = $15,
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
    : nullableAgeFrom team.ageSpan
    : nullableAgeTo team.ageSpan
    : team.locations
    : team.languages
    : team.microphone
    : toNullable team.timezone
    : nullableTimeFrom team.onlineWeekday
    : nullableTimeTo team.onlineWeekday
    : nullableTimeFrom team.onlineWeekend
    :| nullableTimeTo team.onlineWeekend

updateTeam :: ∀ querier errors. Querier querier =>
    querier -> Id -> String -> Team -> Async (InternalTerror_ errors) Unit
updateTeam pool ownerId handle team =
    queryNone pool queryString (queryParameters ownerId handle team)

update :: ∀ left. Pool -> Cookies -> { handle :: String } -> UpdateTeam.RequestContent -> Async left _
update pool cookies { handle } content =
    sendResponse "Error updating team" do
    { cookieInfo } <- ensureSignedInOwner pool cookies handle
    team <- validateTeam content
    updateTeam pool cookieInfo.id handle team
    pure noContent_
