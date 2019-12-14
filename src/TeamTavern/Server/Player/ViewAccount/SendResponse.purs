module TeamTavern.Server.Player.ViewAccount.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.ViewAccount.LoadAccount (LoadAccountResult)
import TeamTavern.Server.Player.ViewAccount.LogError (ViewAccountError)

type OkContent = LoadAccountResult

errorResponse :: ViewAccountError -> Response
errorResponse = match
    { noCookieInfo: const badRequest__
    , invalidSession: const badRequest__
    , nicknameDoesntMatch: const badRequest__
    , notFound: const badRequest__
    , unreadableAccount: const internalServerError__
    , databaseError: const internalServerError__
    }

successResponse :: LoadAccountResult -> Response
successResponse conversations = ok_ $ writeJSON conversations

sendResponse
    :: Async ViewAccountError LoadAccountResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse