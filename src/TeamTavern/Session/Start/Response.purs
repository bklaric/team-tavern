module TeamTavern.Session.Start.Response
    (NicknamedNonceErrorContent, BadRequestContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent)
import Simple.JSON (writeJSON)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Session.Start.Types (StartError)

type NicknamedNonceErrorContent = Variant
    ( invalidNickname :: {}
    , invalidNonce :: {}
    )

type BadRequestContent = Variant
    ( invalidNicknamedNonce :: Array NicknamedNonceErrorContent
    , noTokenToConsume :: {}
    )

errorResponse :: StartError -> Response
errorResponse = match
    { unreadableNicknamedNonce: const $ badRequest__
    , invalidNicknamedNonce: \{ errors } ->
        errors
        <#> (match
            { nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            , nonce: const $ inj (SProxy :: SProxy "invalidNonce") {}
            })
        # Array.fromFoldable
        # inj (SProxy :: SProxy "invalidNicknamedNonce")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , noTokenToConsume: const $ badRequest_ $ writeJSON
        (inj (SProxy :: SProxy "noTokenToConsume") {} :: BadRequestContent)
    , databaseError: const internalServerError__
    , unreadableIdentifiedToken: const internalServerError__
    , invalidIdentifiedToken: const internalServerError__
    }

successResponse :: AuthInfo -> Response
successResponse { id, nickname, token } =
    noContent $ setCookieHeader id nickname token

response ::
    Async StartError AuthInfo -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
