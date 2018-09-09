module TeamTavern.Player.Update.UpdatePlayer
    (UpdatePlayerError, updatePlayer) where

import Prelude

import Async (Async)
import Async (left) as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.Types (AuthInfo, NicknamedAbout)

updatePlayerQuery :: Query
updatePlayerQuery = Query """
    update player
    set nickname = $3, about = $4
    from session
    where player.id = session.player_id
    and session.player_id = $1
    and session.token = $2
    and session.consumed = true
    and session.revoked = false
    """

updatePlayerQueryParameters ::
    AuthInfo -> NicknamedAbout -> Array QueryParameter
updatePlayerQueryParameters { id, token } { nickname, about} =
    [toString id, unwrap token, unwrap nickname, unwrap about]
    <#> QueryParameter

type UpdatePlayerError errors = Variant
  ( nicknameTaken ::
    { nickname :: Nickname
    , error :: Error
    }
  , databaseError :: Error
  , notAuthorized ::
    { authInfo :: AuthInfo
    , nickname :: Nickname
    }
  | errors )

updatePlayer
    :: forall errors
    .  Pool
    -> AuthInfo
    -> NicknamedAbout
    -> Async (UpdatePlayerError errors) Unit
updatePlayer pool authInfo nicknamedAbout = do
    result <- pool
        # query updatePlayerQuery
            (updatePlayerQueryParameters authInfo nicknamedAbout)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken")
                { nickname: nicknamedAbout.nickname, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    if rowCount result == 1
        then pure unit
        else Async.left $ inj (SProxy :: SProxy "notAuthorized")
            { authInfo, nickname: nicknamedAbout.nickname }
