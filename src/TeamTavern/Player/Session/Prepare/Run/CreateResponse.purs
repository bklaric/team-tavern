module TeamTavern.Player.Session.Prepare.Run.CreateResponse
    ( IdentifiersErrorResponseContent
    , BadRequestResponseContent
    , prepareResponse
    ) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import MultiMap (empty)
import Perun.Response (Response)
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
    { ensureNotSignedIn: const
        { statusCode: 403
        , headers: empty
        , content: mempty
        }
    , readIdentifiers: const
        { statusCode: 400
        , headers: empty
        , content: mempty
        }
    , validateIdentifiers: \{ errors } ->
        { statusCode: 400
        , headers: empty
        , content: errors <#> (match
            { email: const invalidEmail
            , nickname: const invalidNickname
            })
            # fromFoldable
            # invalidIdentifiers
            # writeJSON
        }
    , generateSecrets: const
        { statusCode: 500
        , headers: empty
        , content: mempty
        }
    , createSession: _.error >>> match
        { unknownIdentifiers : const
            { statusCode: 400
            , headers: empty
            , content: writeJSON unknownIdentifiers
            }
        , other: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
        }
    , notifyPlayer: const
        { statusCode: 500
        , headers: empty
        , content: mempty
        }
    }

successResponse :: Response
successResponse =
    { statusCode: 204
    , headers: empty
    , content: mempty
    }

prepareResponse ::
    Async PrepareError Unit -> (forall left. Async left Response)
prepareResponse = alwaysRight errorResponse (const successResponse)
