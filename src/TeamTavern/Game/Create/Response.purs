module TeamTavern.Game.Create.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Create.Types (CreateError)

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

errorResponse :: CreateError -> Response
errorResponse = match
    { cookiesNotPresent: const $ unauthorized__
    , cantReadDetailsModel: const $ badRequest__
    , cantValidateDetails: \{ errors } ->
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

response :: Async CreateError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
