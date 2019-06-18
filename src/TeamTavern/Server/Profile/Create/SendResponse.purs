module TeamTavern.Server.Profile.Create.SendResponse
    (ProfileErrorContent, BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj, match, on)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.Create.LogError (CreateError)

type ProfileErrorContent = Variant
    ( invalidSummary :: {}
    , invalidUrl :: { fieldId :: Int }
    )

type BadRequestContent = Variant
    ( invalidProfile :: Array ProfileErrorContent )

errorResponse :: CreateError -> Response
errorResponse = match
    { cookieInfoNotPresent: const unauthorized__
    , databaseError: const internalServerError__
    , unreadableFieldDtos: const internalServerError__
    , invalidFieldModels: const internalServerError__
    , unreadableProfileDto: const badRequest__
    , invalidProfileModel: \{ errors } ->
        errors
        <#> match
            { summary: const $ Array.singleton
                $ inj (SProxy :: SProxy "invalidSummary") {}
            , fieldValues: \fieldValueErrors ->
                fieldValueErrors
                <#> on (SProxy :: SProxy "invalidUrl")
                    (\{ fieldValueDto: { fieldId } } ->
                        Just $ inj (SProxy :: SProxy "invalidUrl") { fieldId })
                    (const Nothing)
                # Array.fromFoldable
                # Array.catMaybes
            }
        # Array.fromFoldable
        # join
        # inj (SProxy :: SProxy "invalidProfile")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , notAuthorized: const forbidden__
    , unreadableProfileId: const internalServerError__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async CreateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
