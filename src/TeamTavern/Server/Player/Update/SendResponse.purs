module TeamTavern.Server.Player.Update.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.Update.LogError (UpdateError)

type BadRequestContent = Variant
    ( invalidModel ::
        Array (Variant
        ( invalidDiscordTag :: {}
        ))
    , nicknameTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , databaseError: const $ internalServerError__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const unauthorized__
    , unreadableDto: const $ badRequest__
    , invalidModel: \{ errors } ->
        errors
        <#> (match
            { discordTag: const $ inj (SProxy :: SProxy "invalidDiscordTag") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidModel")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , nicknameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "nicknameTaken") {}
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse :: Async UpdateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
