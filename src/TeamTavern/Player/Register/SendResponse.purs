module TeamTavern.Player.Register.SendResponse
    ( OkContent
    , BadRequestContent
    , IdentifiersErrorContent
    , sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Register.LogError (RegisterError)
import TeamTavern.Player.Register.ValidateModel (Email, Nickname, unEmail, unNickname)

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
        ({ email: unEmail email
        , nickname: unNickname nickname
        , emailSent: false
        } :: OkContent)
    }

successResponse :: SendResponseModel -> Response
successResponse { email, nickname } =
    ok_ $ writeJSON
    ({ email: unEmail email
    , nickname: unNickname nickname
    , emailSent: true
    } :: OkContent)

sendResponse ::
    Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
