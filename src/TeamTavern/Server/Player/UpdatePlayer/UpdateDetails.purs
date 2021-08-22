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
        discord_tag = $2,
        birthday = $3,
        languages = $4,
        location = $5,
        timezone = $6,
        weekday_from = $7::time,
        weekday_to = $8::time,
        weekend_from = $9::time,
        weekend_to = $10::time,
        microphone = $11
    where player.id = $1
    """

queryParameters :: Int -> Player -> Array QueryParameter
queryParameters playerId model =
    playerId
    : toNullable model.discordTag
    : toNullable model.birthday
    : model.languages
    : toNullable model.location
    : toNullable model.timezone
    : nullableTimeFrom model.onlineWeekday
    : nullableTimeTo model.onlineWeekday
    : nullableTimeFrom model.onlineWeekend
    : nullableTimeTo model.onlineWeekend
    :| model.microphone

updateDetails :: forall querier errors. Querier querier =>
    querier -> Int -> Player -> Async (InternalError errors) Unit
updateDetails querier playerId updateModel =
    queryNone querier queryString (queryParameters playerId updateModel)
