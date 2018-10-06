module TeamTavern.Profile.View.Response (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.Domain.Types (View)
import TeamTavern.Profile.View.Types (ViewError)

type OkContent =
    { summary :: String
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { invalidIdentifiers: const $ notFound__
    , databaseError: const $ internalServerError__
    , unreadableViewModel: const $ internalServerError__
    , notFound: const $ notFound__
    , invalidView: const $ internalServerError__
    }

successResponse :: View -> Response
successResponse { summary } = ok_ $ writeJSON (
    { summary: unwrap summary
    } :: OkContent)

response :: Async ViewError View -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
