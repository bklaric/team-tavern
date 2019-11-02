module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

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
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Player.Domain.Email (Email)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Domain.Nonce (Nonce)

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

queryString :: Query
queryString = Query """
    insert into player (email, nickname, password_hash, confirmation_nonce)
    values ($1, $2, $3, $4)
    """

queryParameters :: AddPlayerModel -> Array QueryParameter
queryParameters { email, nickname, hash, nonce } =
    email : nickname : hash :| nonce

addPlayer
    :: forall errors
    .  Pool
    -> AddPlayerModel
    -> Async (AddPlayerError errors) Unit
addPlayer pool model @ { email, nickname, nonce } =
    pool
    # execute queryString (queryParameters model)
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_lower_email_key" ->
            inj (SProxy :: SProxy "emailTaken") { email, error }
        true | constraint error == Just "player_lower_nickname_key" ->
            inj (SProxy :: SProxy "nicknameTaken") { nickname, error }
        _ -> inj (SProxy :: SProxy "databaseError") error )
