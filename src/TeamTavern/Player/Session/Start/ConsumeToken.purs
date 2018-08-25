module TeamTavern.Player.Session.Start.ConsumeToken
    (ConsumeTokenError, consumeToken) where

import Prelude

import Async (Async, fromEither)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (Foreign, ForeignError)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON (read)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (IdentifiedToken', NicknamedNonce)

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

updateTokenParameters :: Nickname -> Nonce -> Array QueryParameter
updateTokenParameters nickname nonce =
    [unwrap nickname, unwrap nonce] <#> QueryParameter

type ConsumeTokenError =
    { nickname :: Nickname
    , nonce :: Nonce
    , error :: Variant
        ( noTokenToConsume :: Result
        , cantReadIdentifiedToken :: Result
        , other :: Error
        )
    }

_noTokenToConsume = SProxy :: SProxy "noTokenToConsume"

_cantReadIdentifiedToken = SProxy :: SProxy "cantReadIdentifiedToken"

_other = SProxy :: SProxy "other"

_consumeToken = SProxy :: SProxy "consumeToken"

readIdentifiedToken ::
    NicknamedNonce -> Result -> Async ConsumeTokenError IdentifiedToken'
readIdentifiedToken { nickname, nonce } result =
    result
    # rows
    # traverse (read
        :: Foreign
        -> Either (NonEmptyList ForeignError) { id :: Int, token :: String })
    # case _ of
        Right tokens ->
            case head tokens of
            Just { id, token } ->
                note (inj _cantReadIdentifiedToken result) do
                    id' <- PlayerId.create id
                    token' <- Token.create token # hush
                    pure { id: id', nickname, token: token' }
            Nothing -> Left $ inj _noTokenToConsume result
        _ -> Left $ inj _cantReadIdentifiedToken result
    # lmap { error: _, nickname, nonce }
    # fromEither

consumeToken
    :: forall errors
    .  Pool
    -> NicknamedNonce
    -> Async
        (Variant (consumeToken :: ConsumeTokenError | errors))
        IdentifiedToken'
consumeToken pool nicknamedNonce@{ nickname, nonce } = label _consumeToken do
    result <- query updateTokenQuery (updateTokenParameters nickname nonce) pool
        # label _other
        # lmap { error: _, nickname, nonce }
    readIdentifiedToken nicknamedNonce result
