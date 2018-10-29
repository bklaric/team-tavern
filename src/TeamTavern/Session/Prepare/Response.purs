module TeamTavern.Session.Prepare.Response
    ( IdentifiersErrorContent
    , BadRequestContent
    , response
    ) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Session.Prepare.Types (PrepareError)

type IdentifiersErrorContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    )

type BadRequestContent = Variant
    ( invalidIdentifiers :: Array IdentifiersErrorContent
    , unknownIdentifiers :: {}
    )

invalidEmail :: IdentifiersErrorContent
invalidEmail = inj (SProxy :: SProxy "invalidEmail") {}

invalidNickname :: IdentifiersErrorContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidIdentifiers :: Array (IdentifiersErrorContent) -> BadRequestContent
invalidIdentifiers = inj (SProxy :: SProxy "invalidIdentifiers")

unknownIdentifiers :: BadRequestContent
unknownIdentifiers = inj (SProxy :: SProxy "unknownIdentifiers") {}

errorResponse :: PrepareError -> Response
errorResponse = match
    { signedIn: const forbidden__
    , unreadableIdentifiers: const badRequest__
    , invalidIdentifiers: \{ errors } ->
        errors
        <#> (match
            { email: const $ inj (SProxy :: SProxy "invalidEmail") {}
            , nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidIdentifiers")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , randomError: const internalServerError__
    , invalidGeneratedNonce: const internalServerError__
    , invalidGeneratedToken: const internalServerError__
    , databaseError: const internalServerError__
    , unknownIdentifiers: const $ badRequest_ $ writeJSON
        (inj (SProxy :: SProxy "unknownIdentifiers") {} :: BadRequestContent)
    , sendEmailError: const internalServerError__
    }

successResponse :: Response
successResponse = noContent_

response :: Async PrepareError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse (const successResponse)
