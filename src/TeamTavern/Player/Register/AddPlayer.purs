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
import Postgres.Query (Query(..), QueryParameter, toQueryParameter)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Hash (Hash)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)

type AddPlayerModel =
    { email :: Email
    , nickname :: Nickname
    , hash :: Hash
    , nonce :: Nonce
    }

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

addPlayerQuery :: Query
addPlayerQuery = Query """
    insert into player (email, nickname, password_hash, confirmation_nonce)
    values ($1, $2, $3, $4)
    """

addPlayerQueryParameters :: AddPlayerModel -> Array QueryParameter
addPlayerQueryParameters { email, nickname, hash, nonce } =
    [unwrap email, unwrap nickname, unwrap hash, unwrap nonce]
    <#> toQueryParameter

addPlayer
    :: forall errors
    .  Pool
    -> AddPlayerModel
    -> Async (AddPlayerError errors) Unit
addPlayer pool model @ { email, nickname, nonce } =
    pool
    # execute addPlayerQuery (addPlayerQueryParameters model)
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key" ->
            inj (SProxy :: SProxy "emailTaken") { email, error }
        true | constraint error == Just "player_nickname_key" ->
            inj (SProxy :: SProxy "nicknameTaken") { nickname, error }
        _ -> inj (SProxy :: SProxy "databaseError") error )
