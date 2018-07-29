module TeamTavern.Player.Register.Run.CreateResponse (registerResponse) where

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

type IdentifiersErrorResponseContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    )

type ErrorResponseContent = Variant
    ( invalidIdentifiers :: Array IdentifiersErrorResponseContent
    , emailTaken :: {}
    , nicknameTaken :: {}
    , couldntSendEmail :: {}
    , other :: {}
    )

invalidEmail :: IdentifiersErrorResponseContent
invalidEmail = inj (SProxy :: SProxy "invalidEmail") {}

invalidNickname :: IdentifiersErrorResponseContent
invalidNickname = inj (SProxy :: SProxy "invalidNickname") {}

invalidIdentifiers ::
    Array (IdentifiersErrorResponseContent) -> ErrorResponseContent
invalidIdentifiers = inj (SProxy :: SProxy "invalidIdentifiers")

emailTaken :: ErrorResponseContent
emailTaken = inj (SProxy :: SProxy "emailTaken") {}

nicknameTaken :: ErrorResponseContent
nicknameTaken = inj (SProxy :: SProxy "nicknameTaken") {}

couldntSendEmail :: ErrorResponseContent
couldntSendEmail = inj (SProxy :: SProxy "couldntSendEmail") {}

other :: ErrorResponseContent
other = inj (SProxy :: SProxy "other") {}

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
        , content: writeJSON other
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
    , sendEmail: const
        { statusCode: 500
        , headers: empty
        , content: writeJSON couldntSendEmail
        }
    }

successResponse :: Credentials -> Response
successResponse { email, nickname } =
    { statusCode: 200
    , headers: empty
    , content: writeJSON
        { email: unwrap email
        , nickname: unwrap nickname
        , sendEmailError: false
        }
    }

registerResponse ::
    Async RegisterError Credentials -> (forall left. Async left Response)
registerResponse = alwaysRight errorResponse successResponse
