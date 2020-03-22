module TeamTavern.Server.Player.EditSettings.UpdateSettings
    (UpdateSettingsError, updateSettings) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (execute)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.EditSettings.ReadSettings (EditSettingsModel)

type UpdateSettingsError errors = Variant
  ( databaseError :: Error
  | errors )

queryString :: Query
queryString = Query """
    update player
    set notify = $2
    where player.id = $1
    """

queryParameters :: CookieInfo -> EditSettingsModel -> Array QueryParameter
queryParameters { id } { notify } = id :| notify

updateSettings
    :: forall errors
    .  Pool
    -> CookieInfo
    -> EditSettingsModel
    -> Async (UpdateSettingsError errors) Unit
updateSettings pool cookieInfo updateModel = do
    pool
        # execute queryString (queryParameters cookieInfo updateModel)
        # lmap (inj (SProxy :: SProxy "databaseError"))
    pure unit
