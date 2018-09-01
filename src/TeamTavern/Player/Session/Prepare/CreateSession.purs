module TeamTavern.Player.Session.Prepare.CreateSession
    (CreateSessionError, createSession) where

import Prelude

import Async (Async, left)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rowCount)
import Data.Bifunctor.Label (label)
import Postgres.Async.Query (query)
import TeamTavern.Player.Domain.Types (Credentials)

createSessionQuery :: Query
createSessionQuery = Query """
    insert into session (player_id, token, nonce)
    select player.id, $3, $4
    from player
    where player.email = $1 and player.nickname = $2
    """

createSessionParameters :: Credentials -> Array QueryParameter
createSessionParameters { email, nickname, token, nonce } =
    [unwrap email, unwrap nickname, unwrap token, unwrap nonce]
    <#> QueryParameter

type CreateSessionError =
    { credentials :: Credentials
    , error :: Variant
        ( unknownIdentifiers :: Result
        , other :: Error
        )
    }

createSession
    :: forall errors
    .  Pool
    -> Credentials
    -> Async (Variant (createSession :: CreateSessionError | errors)) Unit
createSession pool credentials = label (SProxy :: SProxy "createSession") do
    result <- pool
        # query createSessionQuery (createSessionParameters credentials)
        # label (SProxy :: SProxy "other")
        # lmap { error: _, credentials }
    case rowCount result of
        1 -> pure unit
        _ -> left
            { error: inj (SProxy :: SProxy "unknownIdentifiers") result
            , credentials
            }
