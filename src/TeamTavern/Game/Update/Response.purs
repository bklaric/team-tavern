module TeamTavern.Game.Update.Response
    (DetailsErrorContent, BadRequestContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, notFound__, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Update.Types (UpdateError)

type DetailsErrorContent = Variant
    ( invalidName :: {}
    , invalidHandle :: {}
    , invalidDescription :: {}
    )

type BadRequestContent = Variant
    ( invalidDetails :: Array DetailsErrorContent
    , nameTaken :: {}
    , handleTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { invalidHandle: const notFound__
    , authNotPresent: const unauthorized__
    , unreadableDetails: const badRequest__
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
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , nameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "nameTaken") {}
    , handleTaken: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "handleTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

response :: Async UpdateError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
