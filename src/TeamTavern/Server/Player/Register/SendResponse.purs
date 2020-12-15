module TeamTavern.Server.Player.Register.SendResponse (OkContent, BadRequestContent, IdentifiersErrorContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Register.LogError (RegisterError)

type SendResponseModel = { nickname :: Nickname }

type OkContent = { nickname :: String }

type IdentifiersErrorContent = Variant
    ( invalidNickname :: {}
    , invalidPassword :: {}
    )

type BadRequestContent = Variant
    ( registration :: Array IdentifiersErrorContent
    , nicknameTaken :: {}
    )

errorResponse :: RegisterError -> Response
errorResponse = match
    { internal: const internalServerError__
    , signedIn: const forbidden__
    , unreadableDto: const badRequest__
    , registration: \errors ->
        errors
        <#> (match
            { nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            , password: const $ inj (SProxy :: SProxy "invalidPassword") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "registration")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , randomError: const internalServerError__
    , nicknameTaken: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "nicknameTaken") {} :: BadRequestContent)
    , databaseError: const internalServerError__
    , cantReadId: const internalServerError__
    }

successResponse :: SendResponseModel -> Response
successResponse { nickname } = ok_ $ writeJSON ({ nickname: unwrap nickname } :: OkContent)

sendResponse ::
    Async RegisterError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
