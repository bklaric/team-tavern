module TeamTavern.Session.Prepare.CreateSession
    (CreateSessionError, createSession) where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
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

type CreateSessionError errors = Variant
    ( databaseError :: Error
    , unknownIdentifiers ::
        { email :: Email
        , nickname :: Nickname
        }
    | errors )

createSession :: forall errors.
    Pool -> Credentials -> Async (CreateSessionError errors) Unit
createSession pool credentials @ { email, nickname } = do
    result <- pool
        # query createSessionQuery (createSessionParameters credentials)
        # label (SProxy :: SProxy "databaseError")
    case rowCount result of
        1 -> pure unit
        _ -> left $ inj (SProxy :: SProxy "unknownIdentifiers")
            { email, nickname }
