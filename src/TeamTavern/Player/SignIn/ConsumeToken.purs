module TeamTavern.Player.SignIn.ConsumeToken where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(SProxy), Variant)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rowCount)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Nickname (Nickname)
import TeamTavern.Player.Token (Token)

updateTokenQuery :: Query
updateTokenQuery = Query """
    update token
    set consumed = true
    where exists (
        select true
        from player
        where
            player.nickname = $1
            and token.player_id = player.id
            and token.value = $2
            and token.consumed = false
            and token.generated > (now() - interval '15 minutes')
    )
    """

updateTokenParameters :: Nickname -> Token -> Array QueryParameter
updateTokenParameters nickname token =
    [unwrap nickname, unwrap token] <#> QueryParameter

type ConsumeTokenError = Variant
    ( noTokenToConsume ::
        { nickname :: Nickname, token :: Token }
    , other ::
        { error :: Error, nickname :: Nickname, token :: Token }
    )

_noTokenToConsume = SProxy :: SProxy "noTokenToConsume"

_other = SProxy :: SProxy "other"

consumeToken
    :: forall errors
    .  Pool
    -> Nickname
    -> Token
    -> Async (Variant (consumeToken :: ConsumeTokenError | errors)) Unit
consumeToken pool nickname token = label (SProxy :: SProxy "consumeToken") do
    result <- query updateTokenQuery (updateTokenParameters nickname token) pool
        # lmap { error: _, nickname, token }
        # label _other
    if rowCount result > 0
        then pure unit
        else { nickname, token } # Left # fromEither # label _noTokenToConsume
