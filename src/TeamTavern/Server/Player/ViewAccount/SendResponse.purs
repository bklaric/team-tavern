module TeamTavern.Server.Player.ViewAccount.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Maybe (Maybe)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.ViewAccount.LoadAccount (LoadAccountResult)
import TeamTavern.Server.Player.ViewAccount.LogError (ViewAccountError)

type OkContent =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , weekdayStart :: Maybe String
    , weekdayEnd :: Maybe String
    , weekendStart :: Maybe String
    , weekendEnd :: Maybe String
    , hasMicrophone :: Boolean
    }

errorResponse :: ViewAccountError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const unauthorized__
    , notFound: const badRequest__
    , unreadableAccount: const internalServerError__
    , databaseError: const internalServerError__
    }

successResponse :: LoadAccountResult -> Response
successResponse account = ok_ $ writeJSON account

sendResponse
    :: Async ViewAccountError LoadAccountResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
