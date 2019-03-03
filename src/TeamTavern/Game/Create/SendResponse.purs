module TeamTavern.Game.Create.SendResponse
    (DetailsErrorContent, BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Create.LogError (CreateError)

type DetailsErrorContent = Variant
    ( invalidTitle :: {}
    , invalidHandle :: {}
    , invalidDescription :: {}
    )

type BadRequestContent = Variant
    ( invalidDetails :: Array DetailsErrorContent
    , titleTaken :: {}
    , handleTaken :: {}
    )

errorResponse :: CreateError -> Response
errorResponse = match
    { cookieInfoNotPresent: const $ unauthorized__
    , unreadableDto: const $ badRequest__
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
successResponse _ = noContent_

sendResponse :: Async CreateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
