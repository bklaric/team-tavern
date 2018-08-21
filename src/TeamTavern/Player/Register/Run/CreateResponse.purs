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
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_)
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
    { ensureNotSignedIn: const forbidden__
    , readIdentifiers: const badRequest__
    , validateIdentifiers: \{ errors } ->
        errors
        <#> (match
            { email: const invalidEmail
            , nickname: const invalidNickname
            })
        # fromFoldable
        # invalidIdentifiers
        # writeJSON
        # badRequest_
    , generateSecrets: const internalServerError__
    , addPlayer: _.error >>> match
        { emailTaken: const $ badRequest_ $ writeJSON emailTaken
        , nicknameTaken: const $ badRequest_ $ writeJSON nicknameTaken
        , other: const internalServerError__
       }
    , sendEmail: _.identifiers >>> \{ email, nickname } ->
        ok_ $ writeJSON
        { email: unwrap email
        , nickname: unwrap nickname
        , emailSent: false
        }
    }

successResponse :: Credentials -> Response
successResponse { email, nickname } =
    ok_ $ writeJSON
    { email: unwrap email
    , nickname: unwrap nickname
    , emailSent: true
    }

registerResponse ::
    Async RegisterError Credentials -> (forall left. Async left Response)
registerResponse = alwaysRight errorResponse successResponse
