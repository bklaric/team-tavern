module TeamTavern.Profile.View.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.View.LoadProfile (LoadProfileResult)
import TeamTavern.Profile.View.LogError (ViewError)

type OkContent = { summary :: Array String }

errorResponse :: ViewError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableDto: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadProfileResult -> Response
successResponse { summary } = ok_ $ writeJSON (
    { summary: unwrap summary <#> unwrap
    } :: OkContent)

sendResponse ::
    Async ViewError LoadProfileResult -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
