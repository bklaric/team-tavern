module TeamTavern.Server.Team.Create.AddTeam (addTeam) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (Handle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (Team, organizationName, organizationWebsite, toString)

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
        , organization
        , name
        , website
        , age_from
        , age_to
        , locations
        , languages
        , microphone
        , timezone
        , weekday_from
        , weekday_to
        , weekend_from
        , weekend_to
        )
    values
        ( $1, (select handle from unique_handle)
        , $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15
        )
    returning team.id, team.handle;
    """

queryParameters :: Id -> Handle -> Team -> Array QueryParameter
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

addTeam :: forall querier errors. Querier querier =>
    querier -> Id -> Handle -> Team -> Async (InternalError errors) { id :: Int, handle :: String }
addTeam pool ownerId handle team =
    queryFirstInternal pool queryString (queryParameters ownerId handle team)
