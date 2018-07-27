module TeamTavern.Player.StartSession.ConsumeToken where

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
import TeamTavern.Player.StartSession.Types.IdentifiedToken (IdentifiedToken)
import TeamTavern.Player.StartSession.Types.NicknamedNonce (NicknamedNonce)

updateTokenQuery :: Query
updateTokenQuery = Query """
    update token
    set consumed = true
    from player
    where player.nickname = $1
        and token.player_id = player.id
        and token.nonce = $2
        and token.consumed = false
        and token.revoked = false
        and token.generated > (now() - interval '15 minutes')
    returning player.id as id, token.value as token
    """

updateTokenParameters :: Nickname -> Nonce -> Array QueryParameter
updateTokenParameters nickname nonce =
    [unwrap nickname, unwrap nonce] <#> QueryParameter

type ConsumeTokenError = Variant
    ( noTokenToConsume ::
        { nickname :: Nickname, nonce :: Nonce }
    , cantReadIdentifiedToken ::
        { result :: Result, nickname :: Nickname, nonce :: Nonce }
    , other ::
        { error :: Error, nickname :: Nickname, nonce :: Nonce }
    )

_noTokenToConsume = SProxy :: SProxy "noTokenToConsume"

_cantReadIdentifiedToken = SProxy :: SProxy "cantReadIdentifiedToken"

_other = SProxy :: SProxy "other"

_consumeToken = SProxy :: SProxy "consumeToken"

readIdentifiedToken ::
    NicknamedNonce -> Result -> Async ConsumeTokenError IdentifiedToken
readIdentifiedToken { nickname, nonce } result =
    result
    # rows
    # traverse (read
        :: Foreign
        -> Either (NonEmptyList ForeignError) { id :: Int, token :: String })
    # case _ of
        Right tokens | Just { id, token } <- head tokens ->
            note (inj _cantReadIdentifiedToken { result, nickname, nonce }) do
                id' <- PlayerId.create id
                token' <- Token.create token # hush
                pure { id: id', token: token' }
        _ -> Left $ inj _cantReadIdentifiedToken { result, nickname, nonce }
    # fromEither

consumeToken
    :: forall errors
    .  Pool
    -> NicknamedNonce
    -> Async
        (Variant (consumeToken :: ConsumeTokenError | errors))
        IdentifiedToken
consumeToken pool nicknamedNonce@{ nickname, nonce } = label _consumeToken do
    result <- query updateTokenQuery (updateTokenParameters nickname nonce) pool
        # lmap { error: _, nickname, nonce }
        # label _other
    readIdentifiedToken nicknamedNonce result
