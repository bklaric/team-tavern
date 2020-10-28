module TeamTavern.Server.Player.UpdateDetails.UpdateDetails
    (UpdateDetailsError, updateDetails) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Nullable (toNullable)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (execute)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Player.UpdateDetails.ReadUpdate (UpdateDetailsModel)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (nullableTimeFrom, nullableTimeTo)

type UpdateDetailsError errors = Variant (databaseError :: Error | errors)

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
        microphone = $11,
        about = $12
    where player.id = $1
    """

queryParameters :: Int -> UpdateDetailsModel -> Array QueryParameter
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
    : model.microphone
    :| model.about

updateDetails
    :: forall querier errors
    .  Querier querier
    => querier
    -> Int
    -> UpdateDetailsModel
    -> Async (UpdateDetailsError errors) Unit
updateDetails querier playerId updateModel =
    querier
    # execute queryString (queryParameters playerId updateModel)
    # lmap (inj (SProxy :: SProxy "databaseError"))
