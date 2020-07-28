module TeamTavern.Server.Player.ChangeNickname.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, setCookieHeaderNickname)
import TeamTavern.Server.Player.ChangeNickname.LogError (UpdateError)

type BadRequestContent = Variant
    ( invalidNickname :: {}
    , nicknameTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const forbidden__
    , unreadableDto: const $ badRequest__
    , invalidModel: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "invalidNickname") {}
    , nicknameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "nicknameTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: CookieInfo -> Response
successResponse cookieInfo = noContent $ setCookieHeaderNickname cookieInfo

sendResponse ::
    Async UpdateError CookieInfo -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
