module TeamTavern.Player.Session.Start.Run.CreateResponse (startResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import MultiMap (empty)
import Perun.Response (Response)
import Simple.JSON (writeJSON)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)
import TeamTavern.Player.Session.Start.Run.Types (StartError)

type ErrorResponseContent = Variant
    ( invalidNickname :: {}
    , invalidNonce :: {}
    , noTokenToConsume :: {}
    )

invalidNickname :: ErrorResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidNonce :: ErrorResponseContent
invalidNonce = inj (SProxy :: SProxy "invalidNonce") {}

noTokenToConsume :: ErrorResponseContent
noTokenToConsume = inj (SProxy :: SProxy "noTokenToConsume") {}

errorResponse :: StartError -> Response
errorResponse = match
    { ensureNotSignedIn: const
        { statusCode: 403
        , headers: empty
        , content: mempty
        }
    , readNickname: const
        { statusCode: 400
        , headers: empty
        , content: writeJSON invalidNickname
        }
    , readNonce: const
        { statusCode: 400
        , headers: empty
        , content: writeJSON invalidNonce
        }
    , consumeToken: _.error >>> match
        { noTokenToConsume: const
            { statusCode: 400
            , headers: empty
            , content: writeJSON noTokenToConsume
            }
        , cantReadIdentifiedToken: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
        , other: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
        }
    }

successResponse :: { id :: PlayerId, token :: Token} -> Response
successResponse { id, token } =
    { statusCode: 204
    , headers: setCookieHeader id token
    , content: mempty
    }

startResponse
    :: Async StartError { id :: PlayerId, token :: Token}
    -> (forall left. Async left Response)
startResponse = alwaysRight errorResponse successResponse
