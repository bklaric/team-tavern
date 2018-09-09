module TeamTavern.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Async.Query (execute)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (Credentials)

addPlayerQuery :: Query
addPlayerQuery = Query """
    with insert_result (player_id) as
    (   insert into player (email, nickname)
        values ($1, $2)
        returning id
    )
    insert into session (player_id, token, nonce)
    select insert_result.player_id, $3, $4
    from insert_result
    """

addPlayerQueryParameters :: Credentials -> Array QueryParameter
addPlayerQueryParameters { email, nickname, token, nonce } =
    [unwrap email, unwrap nickname, unwrap token, unwrap nonce]
    <#> QueryParameter

type AddPlayerError errors = Variant
    ( emailTaken ::
        { email :: Email
        , error :: Error
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Error
        }
    , databaseError :: Error
    | errors )

addPlayer
    :: forall errors
    .  Pool
    -> Credentials
    -> Async (AddPlayerError errors) Unit
addPlayer pool credentials@{ email, nickname, token, nonce } =
    pool
    # execute addPlayerQuery (addPlayerQueryParameters credentials)
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key" ->
            inj (SProxy :: SProxy "emailTaken") { email, error }
        true | constraint error == Just "player_nickname_key" ->
            inj (SProxy :: SProxy "nicknameTaken") { nickname, error }
        _ -> inj (SProxy :: SProxy "databaseError") error )
