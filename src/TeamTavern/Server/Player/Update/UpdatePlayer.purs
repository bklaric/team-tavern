module TeamTavern.Server.Player.Update.UpdatePlayer
    (UpdatePlayerError, updatePlayer) where

import Prelude

import Async (Async)
import Async (left) as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Update.ReadUpdate (UpdateModel)

type UpdatePlayerError errors = Variant
  ( nicknameTaken ::
    { nickname :: Nickname
    , error :: Error
    }
  , databaseError :: Error
  , notAuthorized ::
    { cookieInfo :: CookieInfo
    , nickname :: Nickname
    }
  | errors )

queryString :: Query
queryString = Query """
    update player
    set
        nickname = $3,
        discord_tag = $4,
        birthday = $5,
        has_microphone = $6,
        about = $7,
        notify = $8
    from session
    where player.id = session.player_id
    and session.player_id = $1
    and session.token = $2
    and session.revoked = false
    """

queryParameters :: CookieInfo -> UpdateModel -> Array QueryParameter
queryParameters { id, token } { nickname, discordTag, birthday, hasMicrophone, about, notify } =
    id : token : nickname : toNullable discordTag : toNullable birthday : hasMicrophone : about :| notify

updatePlayer
    :: forall errors
    .  Pool
    -> CookieInfo
    -> UpdateModel
    -> Async (UpdatePlayerError errors) CookieInfo
updatePlayer pool cookieInfo updateModel = do
    result <- pool
        # query queryString (queryParameters cookieInfo updateModel)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken")
                { nickname: updateModel.nickname, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized")
            { cookieInfo, nickname: updateModel.nickname }
    pure $ cookieInfo { nickname = updateModel.nickname}
