module TeamTavern.Player.Update.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, notFound__, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Update.Types (UpdateError)

type AboutErrorResponseContent = Variant
    ( invalidNickname :: {}
    , invalidAbout :: {}
    )

type BadRequestResponseContent = Variant
    ( invalidIdentifiers :: Array AboutErrorResponseContent
    , nicknameTaken :: {}
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { cantValidateTargetNickname: const $ notFound__
    , cookiesNotPresent: const $ unauthorized__
    , nicknamesNotSame: const $ forbidden__
    , cantReadUpdateModel: const $ badRequest__
    , cantValidateUpdate: \errors ->
        errors
        <#> (match
            { nickname: const $ inj (SProxy :: SProxy "invalidNickname") {}
            , about: const $ inj (SProxy :: SProxy "invalidAbout") {}
            })
        # fromFoldable
        # inj (SProxy :: SProxy "invalidIdentifiers")
        # (writeJSON :: BadRequestResponseContent -> String)
        # badRequest_
    , nicknameTaken: const $ badRequest_
        $ (writeJSON :: BadRequestResponseContent -> String)
        $ inj (SProxy :: SProxy "nicknameTaken") {}
    , databaseError: const $ internalServerError__
    , notAuthorized: const $ forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

response :: Async UpdateError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
