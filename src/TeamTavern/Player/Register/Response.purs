module TeamTavern.Player.Register.Response
    ( OkContent
    , BadRequestContent
    , IdentifiersErrorContent
    , response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Domain.Types (Identifiers)
import TeamTavern.Player.Register.Types (RegisterError)

type OkContent =
    { email :: String
    , nickname :: String
    , emailSent :: Boolean
    }

type IdentifiersErrorContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    )

type BadRequestContent = Variant
    ( invalidIdentifiers :: Array IdentifiersErrorContent
    , emailTaken :: {}
    , nicknameTaken :: {}
    )

errorResponse :: RegisterError -> Response
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
    , emailTaken: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "emailTaken") {} :: BadRequestContent)
    , nicknameTaken: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "nicknameTaken") {} :: BadRequestContent)
    , databaseError: const internalServerError__
    , sendEmailError: _.identifiers >>> \{ email, nickname } ->
        ok_ $ writeJSON
        ({ email: unwrap email
        , nickname: unwrap nickname
        , emailSent: false
        } :: OkContent)
    }

successResponse :: Identifiers -> Response
successResponse { email, nickname } =
    ok_ $ writeJSON
    ({ email: unwrap email
    , nickname: unwrap nickname
    , emailSent: true
    } :: OkContent)

response ::
    Async RegisterError Identifiers -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
