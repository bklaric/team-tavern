module TeamTavern.Player.Update.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Update.LogError (UpdateError)

type BadRequestContent = Variant
    ( invalidIdentifiers :: Array (Variant
        ( invalidNickname :: {}
        , invalidAbout :: {}
        ))
    , nicknameTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { cookieInfoNotPresent: const $ unauthorized__
    , unreadableDto: const $ badRequest__
    , invalidModel: \{ errors } ->
        errors
        <#> (match
            { nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            , about: const $ inj (SProxy :: SProxy "invalidAbout") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidIdentifiers")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , nicknameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "nicknameTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async UpdateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
