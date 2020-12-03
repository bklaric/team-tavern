module TeamTavern.Server.Team.Create.AddTeam (addTeam) where


import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Team.Infrastructure.GenerateHandle (Handle)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (Team)

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
