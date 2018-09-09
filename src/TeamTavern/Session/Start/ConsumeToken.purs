module TeamTavern.Session.Start.ConsumeToken
    (ConsumeTokenError, consumeToken) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (ForeignError)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON (read)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (AuthInfo, NicknamedNonce)
import TeamTavern.Session.Infrastructure.Types (IdentifiedTokenModel)

updateTokenQuery :: Query
updateTokenQuery = Query """
    update session
    set consumed = true
    from player
    where player.nickname = $1
        and session.player_id = player.id
        and session.nonce = $2
        and session.consumed = false
        and session.revoked = false
        and session.generated > (now() - interval '15 minutes')
    returning player.id as id, session.token as token
    """

updateTokenParameters :: NicknamedNonce -> Array QueryParameter
updateTokenParameters { nickname, nonce } =
    [unwrap nickname, unwrap nonce] <#> QueryParameter

type ConsumeTokenError errors = Variant
    ( noTokenToConsume :: NicknamedNonce
    , databaseError :: Error
    , unreadableIdentifiedToken ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , invalidIdentifiedToken :: IdentifiedTokenModel
    | errors )

consumeToken
    :: forall errors
    .  Pool
    -> NicknamedNonce
    -> Async
        (ConsumeTokenError errors)
        AuthInfo
consumeToken pool nicknamedNonce @ { nickname } = do
    result <- pool
        # query updateTokenQuery (updateTokenParameters nicknamedNonce)
        # label (SProxy :: SProxy "databaseError")
    tokens <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableIdentifiedToken")
            { result, errors: _ }
        # Async.fromEither
    { id, token } :: IdentifiedTokenModel <- head tokens
        # Async.note (inj (SProxy :: SProxy "noTokenToConsume") nicknamedNonce)
    { nickname, id: _, token: _ }
        <$> PlayerId.create id
        <*> Token.create'' token
        # Async.note (inj (SProxy :: SProxy "invalidIdentifiedToken")
            { id, token })
