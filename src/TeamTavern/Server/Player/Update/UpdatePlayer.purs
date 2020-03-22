module TeamTavern.Server.Player.Update.UpdatePlayer
    (UpdatePlayerError, updatePlayer) where

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
import TeamTavern.Server.Player.Update.ReadUpdate (UpdateModel)
import TeamTavern.Server.Player.Update.ValidateTimespan (endTime, startTime, toString)

type UpdatePlayerError errors = Variant (databaseError :: Error | errors)

queryString :: Query
queryString = Query """
    update player
    set
        discord_tag = $2,
        birthday = $3,
        languages = $4,
        country = $5,
        timezone = $6,
        weekday_start = $7::time,
        weekday_end = $8::time,
        weekend_start = $9::time,
        weekend_end = $10::time,
        has_microphone = $11
    where player.id = $1
    """

queryParameters :: CookieInfo -> UpdateModel -> Array QueryParameter
queryParameters { id } { discordTag, birthday, languages, country, timezone, onlineWeekday, onlineWeekend, hasMicrophone } =
    id
    : toNullable discordTag
    : toNullable birthday
    : languages
    : toNullable country
    : toNullable timezone
    : (toNullable $ toString <$> startTime <$> onlineWeekday)
    : (toNullable $ toString <$> endTime <$> onlineWeekday)
    : (toNullable $ toString <$> startTime <$> onlineWeekend)
    : (toNullable $ toString <$> endTime <$> onlineWeekend)
    :| hasMicrophone

updatePlayer
    :: forall errors
    .  Pool
    -> CookieInfo
    -> UpdateModel
    -> Async (UpdatePlayerError errors) Unit
updatePlayer pool cookieInfo updateModel = do
    pool
        # execute queryString (queryParameters cookieInfo updateModel)
        # lmap (inj (SProxy :: SProxy "databaseError"))
    pure unit
