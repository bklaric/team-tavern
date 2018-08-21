module TeamTavern.Player.Session.Prepare.Run.CreateResponse
    ( IdentifiersErrorResponseContent
    , BadRequestResponseContent
    , prepareResponse
    ) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Session.Prepare.Run.Types (PrepareError)

type IdentifiersErrorResponseContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    )

type BadRequestResponseContent = Variant
    ( invalidIdentifiers :: Array IdentifiersErrorResponseContent
    , unknownIdentifiers :: {}
    )

invalidEmail :: IdentifiersErrorResponseContent
invalidEmail = inj (SProxy :: SProxy "invalidEmail") {}

invalidNickname :: IdentifiersErrorResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidIdentifiers ::
    Array (IdentifiersErrorResponseContent) -> BadRequestResponseContent
invalidIdentifiers = inj (SProxy :: SProxy "invalidIdentifiers")

unknownIdentifiers :: BadRequestResponseContent
unknownIdentifiers = inj (SProxy :: SProxy "unknownIdentifiers") {}

errorResponse :: PrepareError -> Response
errorResponse = match
    { ensureNotSignedIn: const forbidden__
    , readIdentifiers: const badRequest__
    , validateIdentifiers: \{ errors } ->
        errors <#> (match
            { email: const invalidEmail
            , nickname: const invalidNickname
            })
        # fromFoldable
        # invalidIdentifiers
        # writeJSON
        # badRequest_
    , generateSecrets: const internalServerError__
    , createSession: _.error >>> match
        { unknownIdentifiers: const $ badRequest_ $ writeJSON unknownIdentifiers
        , other: const internalServerError__
        }
    , notifyPlayer: const internalServerError__
    }

successResponse :: Response
successResponse = noContent_

prepareResponse ::
    Async PrepareError Unit -> (forall left. Async left Response)
prepareResponse = alwaysRight errorResponse (const successResponse)
