module TeamTavern.Server.Player.ChangeNickname.UpdateNickname
    (UpdateNicknameError, updateNickname) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (execute)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.ChangeNickname.ReadNickname (ChangeNicknameModel)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type UpdateNicknameError errors = Variant
  ( nicknameTaken ::
    { nickname :: Nickname
    , error :: Error
    }
  , databaseError :: Error
  | errors )

queryString :: Query
queryString = Query """
    update player
    set nickname = $2
    where player.id = $1
    """

queryParameters :: CookieInfo -> ChangeNicknameModel -> Array QueryParameter
queryParameters { id } { nickname } = id :| nickname

updateNickname
    :: forall errors
    .  Pool
    -> CookieInfo
    -> ChangeNicknameModel
    -> Async (UpdateNicknameError errors) CookieInfo
updateNickname pool cookieInfo updateModel = do
    pool
        # execute queryString (queryParameters cookieInfo updateModel)
        # lmap (\error ->
            case code error == unique_violation of
            true | constraint error == Just "player_nickname_key" ->
                inj (SProxy :: SProxy "nicknameTaken")
                { nickname: updateModel.nickname, error }
            _ -> inj (SProxy :: SProxy "databaseError") error)
    pure $ cookieInfo { nickname = updateModel.nickname}
