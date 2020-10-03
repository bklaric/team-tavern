module TeamTavern.Server.Player.UpdateDetails.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.UpdateDetails.LogError (UpdateDetailsError)

type BadRequestContent = Array (Variant
    ( invalidDiscordTag :: {}
    , invalidAbout :: {}
    ))

errorResponse :: UpdateDetailsError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , databaseError: const $ internalServerError__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const forbidden__
    , unreadableDto: const $ badRequest__
    , invalidModel: \{ errors } ->
        errors
        <#> (match
            { discordTag: const $ inj (SProxy :: SProxy "invalidDiscordTag") {}
            , about: const $ inj (SProxy :: SProxy "invalidAbout") {}
            })
        # fromFoldable
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse ::
    Async UpdateDetailsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
