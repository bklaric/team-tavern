module TeamTavern.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Data.Bifunctor.Label (label)
import TeamTavern.Architecture.Postgres.Query (query)
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

type AddPlayerError =
    { credentials :: Credentials
    , error :: Variant
        ( emailTaken :: Error
        , nicknameTaken :: Error
        , other :: Error
        )
    }

_emailTaken = SProxy :: SProxy "emailTaken"

_nicknameTaken = SProxy :: SProxy "nicknameTaken"

_other = SProxy :: SProxy "other"

addPlayer
    :: forall errors
    .  Pool
    -> Credentials
    -> Async (Variant (addPlayer :: AddPlayerError | errors)) Unit
addPlayer pool credentials@{ email, nickname, token, nonce } =
    pool
    # query addPlayerQuery (addPlayerQueryParameters credentials)
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key" ->
            { error: inj _emailTaken error, credentials }
        true | constraint error == Just "player_nickname_key" ->
            { error: inj _nicknameTaken error, credentials }
        _ -> { error: inj _other error, credentials })
    # label (SProxy :: SProxy "addPlayer")
    # void
