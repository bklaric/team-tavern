module TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails) where

import Prelude

import Async (Async)
import Data.Nullable (toNullable)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (Player)
import TeamTavern.Server.Player.UpdatePlayer.ValidateTimespan (nullableTimeFrom, nullableTimeTo)

queryString :: Query
queryString = Query """
    update player
    set
        birthday = $2,
        languages = $3,
        location = $4,
        timezone = $5,
        weekday_from = $6::time,
        weekday_to = $7::time,
        weekend_from = $8::time,
        weekend_to = $9::time,
        microphone = $10
    where player.id = $1
    """

queryParameters :: Int -> Player -> Array QueryParameter
queryParameters playerId model =
    playerId
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
    querier -> Int -> Player -> Async (InternalError_ errors) Unit
updateDetails querier playerId updateModel =
    queryNone querier queryString (queryParameters playerId updateModel)
