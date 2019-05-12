module TeamTavern.Server.Player.Register.SendResponse
    ( OkContent
    , BadRequestContent
    , IdentifiersErrorContent
    , sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.Domain.Email (Email)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Register.LogError (RegisterError)

type SendResponseModel =
    { email :: Email
    , nickname :: Nickname
    }

type OkContent =
    { email :: String
    , nickname :: String
    , emailSent :: Boolean
    }

type IdentifiersErrorContent = Variant
    ( invalidEmail :: {}
    , invalidNickname :: {}
    , invalidPassword :: {}
    )

type BadRequestContent = Variant
    ( invalidModel :: Array IdentifiersErrorContent
    , emailTaken :: {}
    , nicknameTaken :: {}
    )

errorResponse :: RegisterError -> Response
errorResponse = match
    { signedIn: const forbidden__
    , unreadableDto: const badRequest__
    , invalidModel: \{ errors } ->
        errors
        <#> (match
            { email: const $ inj (SProxy :: SProxy "invalidEmail") {}
            , nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            , password: const $ inj (SProxy :: SProxy "invalidPassword") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidModel")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , bcryptError: const internalServerError__
    , randomError: const internalServerError__
    , emailTaken: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "emailTaken") {} :: BadRequestContent)
    , nicknameTaken: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "nicknameTaken") {} :: BadRequestContent)
    , databaseError: const internalServerError__
    , sendEmailError: _.info >>> \{ email, nickname } ->
        ok_ $ writeJSON
        ({ email: unwrap email
        , nickname: unwrap nickname
        , emailSent: false
        } :: OkContent)
    }

successResponse :: SendResponseModel -> Response
successResponse { email, nickname } =
    ok_ $ writeJSON
    ({ email: unwrap email
    , nickname: unwrap nickname
    , emailSent: true
    } :: OkContent)

sendResponse ::
    Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
