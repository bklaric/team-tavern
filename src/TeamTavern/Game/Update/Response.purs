module TeamTavern.Game.Update.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, notFound__, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Update.Types (UpdateError)

type DetailsErrorResponseContent = Variant
    ( invalidName :: {}
    , invalidHandle :: {}
    , invalidDescription :: {}
    )

type BadRequestResponseContent = Variant
    ( invalidDetails :: Array DetailsErrorResponseContent
    , nameTaken :: {}
    , handleTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { invalidTargetHandle: const notFound__
    , cookiesNotPresent: const unauthorized__
    , unreadableDetailsModel: const badRequest__
    , invalidDetails: \{ errors } ->
        errors
        <#> (match
            { name: const $ inj (SProxy :: SProxy "invalidName") {}
            , handle: const $ inj (SProxy :: SProxy "invalidHandle") {}
            , description: const $
                inj (SProxy :: SProxy "invalidDescription") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidDetails")
        # (writeJSON :: BadRequestResponseContent -> String)
        # badRequest_
    , nameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestResponseContent -> String)
        $ inj (SProxy :: SProxy "nameTaken") {}
    , handleTaken: const $ badRequest_
        $ (writeJSON :: BadRequestResponseContent -> String)
        $ inj (SProxy :: SProxy "handleTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

response :: Async UpdateError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
