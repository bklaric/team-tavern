module TeamTavern.Player.Session.Start.Run.CreateResponse
    ( BadRequestResponseContent
    , startResponse
    ) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent)
import Simple.JSON (writeJSON)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Player.Domain.Types (IdentifiedToken')
import TeamTavern.Player.Session.Start.Run.Types (StartError)

type BadRequestResponseContent = Variant
    ( invalidNickname :: {}
    , invalidNonce :: {}
    , noTokenToConsume :: {}
    )

invalidNickname :: BadRequestResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidNonce :: BadRequestResponseContent
invalidNonce = inj (SProxy :: SProxy "invalidNonce") {}

noTokenToConsume :: BadRequestResponseContent
noTokenToConsume = inj (SProxy :: SProxy "noTokenToConsume") {}

errorResponse :: StartError -> Response
errorResponse = match
    { readNickname: const $ badRequest_ $ writeJSON invalidNickname
    , readNonce: match
        { invalidBody: const badRequest__
        , invalidNonce: const $ badRequest_ $ writeJSON invalidNonce
        }
    , consumeToken: _.error >>> match
        { noTokenToConsume: const $ badRequest_ $ writeJSON noTokenToConsume
        , cantReadIdentifiedToken: const internalServerError__
        , other: const internalServerError__
        }
    }

successResponse :: IdentifiedToken' -> Response
successResponse { id, nickname, token } =
    noContent $ setCookieHeader id nickname token

startResponse ::
    Async StartError IdentifiedToken' -> (forall left. Async left Response)
startResponse = alwaysRight errorResponse successResponse
