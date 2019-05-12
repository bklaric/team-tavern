module TeamTavern.Server.Game.Update.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Game.Update.LogError (UpdateError)

type BadRequestContent = Variant
    ( invalidDetails :: Array (Variant
        ( invalidTitle :: {}
        , invalidHandle :: {}
        , invalidDescription :: {}
        ))
    , titleTaken :: {}
    , handleTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { cookieInfoNotPresent: const unauthorized__
    , unreadableDto: const badRequest__
    , invalidModel: \{ errors } ->
        errors
        <#> (match
            { title: const $ inj (SProxy :: SProxy "invalidTitle") {}
            , handle: const $ inj (SProxy :: SProxy "invalidHandle") {}
            , description: const $
                inj (SProxy :: SProxy "invalidDescription") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidDetails")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , titleTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "titleTaken") {}
    , handleTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "handleTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async UpdateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
