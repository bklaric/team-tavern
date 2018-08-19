module TeamTavern.Player.Register.Run.CreateResponse
    ( OkResponseContent
    , BadRequestResponseContent
    , IdentifiersErrorResponseContent
    , registerResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj, match)
import MultiMap (empty)
import Perun.Response (Response)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Domain.Types (Credentials)
import TeamTavern.Player.Register.Run.Types (RegisterError)

type OkResponseContent =
    { email :: String
    , nickname :: String
    , emailSent :: Boolean
    }

type IdentifiersErrorResponseContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    )

type BadRequestResponseContent = Variant
    ( invalidIdentifiers :: Array IdentifiersErrorResponseContent
    , emailTaken :: {}
    , nicknameTaken :: {}
    )

invalidEmail :: IdentifiersErrorResponseContent
invalidEmail = inj (SProxy :: SProxy "invalidEmail") {}

invalidNickname :: IdentifiersErrorResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidIdentifiers ::
    Array (IdentifiersErrorResponseContent) -> BadRequestResponseContent
invalidIdentifiers = inj (SProxy :: SProxy "invalidIdentifiers")

emailTaken :: BadRequestResponseContent
emailTaken = inj (SProxy :: SProxy "emailTaken") {}

nicknameTaken :: BadRequestResponseContent
nicknameTaken = inj (SProxy :: SProxy "nicknameTaken") {}

errorResponse :: RegisterError -> Response
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
    , addPlayer: _.error >>> match
        { emailTaken: const
            { statusCode: 400
            , headers: empty
            , content: writeJSON emailTaken
            }
        , nicknameTaken: const
            { statusCode: 400
            , headers: empty
            , content: writeJSON nicknameTaken
            }
        , other: const
            { statusCode: 500
            , headers: empty
            , content: mempty
            }
       }
    , sendEmail: _.identifiers >>> \{ email, nickname } ->
        { statusCode: 200
        , headers: empty
        , content: writeJSON
            { email: unwrap email
            , nickname: unwrap nickname
            , emailSent: false
            }
        }
    }

successResponse :: Credentials -> Response
successResponse { email, nickname } =
    { statusCode: 200
    , headers: empty
    , content: writeJSON
        { email: unwrap email
        , nickname: unwrap nickname
        , emailSent: true
        }
    }

registerResponse ::
    Async RegisterError Credentials -> (forall left. Async left Response)
registerResponse = alwaysRight errorResponse successResponse
