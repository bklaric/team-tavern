module TeamTavern.Server.Player.UpdatePlayer.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.UpdatePlayer.LogError (UpdateDetailsError)

type BadRequestContent = Array (Variant
    ( invalidDiscordTag :: {}
    , invalidAbout :: {}
    ))

errorResponse :: UpdateDetailsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , databaseError: const $ internalServerError__
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

sendResponse :: Async UpdateDetailsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
