module TeamTavern.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

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
import Postgres.Query (Query(..), QueryParameter(..))
import TeamTavern.Player.Register.GenerateHash (Hash, unHash)
import TeamTavern.Player.Register.GenerateNonce (Nonce, unNonce)
import TeamTavern.Player.Register.ValidateModel (Email, Nickname, unEmail, unNickname)

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
    [unEmail email, unNickname nickname, unHash hash, unNonce nonce]
    <#> QueryParameter

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
