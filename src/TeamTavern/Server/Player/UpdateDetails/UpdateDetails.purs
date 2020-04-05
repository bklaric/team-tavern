module TeamTavern.Server.Player.UpdateDetails.UpdateDetails
    (UpdateDetailsError, updateDetails) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Nullable (toNullable)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (execute)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.UpdateDetails.ReadUpdate (UpdateDetailsModel)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (endTime, startTime, toString)

type UpdateDetailsError errors = Variant (databaseError :: Error | errors)

queryString :: Query
queryString = Query """
    update player
    set
        discord_tag = $2,
        birthday = $3,
        languages = $4,
        country = $5,
        timezone = $6,
        weekday_from = $7::time,
        weekday_to = $8::time,
        weekend_from = $9::time,
        weekend_to = $10::time,
        has_microphone = $11
    where player.id = $1
    """

queryParameters :: CookieInfo -> UpdateDetailsModel -> Array QueryParameter
queryParameters info model =
    info.id
    : toNullable model.discordTag
    : toNullable model.birthday
    : model.languages
    : toNullable model.country
    : toNullable model.timezone
    : (toNullable $ toString <$> startTime <$> model.onlineWeekday)
    : (toNullable $ toString <$> endTime <$> model.onlineWeekday)
    : (toNullable $ toString <$> startTime <$> model.onlineWeekend)
    : (toNullable $ toString <$> endTime <$> model.onlineWeekend)
    :| model.hasMicrophone

updateDetails
    :: forall errors
    .  Pool
    -> CookieInfo
    -> UpdateDetailsModel
    -> Async (UpdateDetailsError errors) Unit
updateDetails pool cookieInfo updateModel =
    pool
    # execute queryString (queryParameters cookieInfo updateModel)
    # lmap (inj (SProxy :: SProxy "databaseError"))
