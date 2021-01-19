module TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (Player)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)

queryString :: Query
queryString = Query """
    update player
    set
        birthday = $2,
        location = $3,
        languages = $4,
        microphone = $5,
        timezone = $6,
        weekday_from = $7::time,
        weekday_to = $8::time,
        weekend_from = $9::time,
        weekend_to = $10::time,
        discord_tag = $11,
        steam_url = $12,
        riot_id = $13,
        about = $14
    where player.id = $1
    """

queryParameters :: Int -> Player -> Array QueryParameter
queryParameters playerId model =
    playerId
    : toNullable model.birthday
    : toNullable model.location
    : model.languages
    : model.microphone
    : toNullable model.timezone
    : nullableTimeFrom model.onlineWeekday
    : nullableTimeTo model.onlineWeekday
    : nullableTimeFrom model.onlineWeekend
    : nullableTimeTo model.onlineWeekend
    : toNullable model.discordTag
    : toNullable model.steamUrl
    : toNullable model.riotId
    :| model.about

updateDetails :: forall querier errors. Querier querier =>
    querier -> Int -> Player -> Async (InternalError errors) Unit
updateDetails querier playerId updateModel =
    queryNone querier queryString (queryParameters playerId updateModel)
