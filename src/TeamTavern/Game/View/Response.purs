module TeamTavern.Game.View.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Game.View.Types (ViewError)
import TeamTavern.Player.Domain.PlayerId (toInt)

errorResponse :: ViewError -> Response
errorResponse = match
    { handleInvalid: const $ notFound__
    , databaseError: const $ internalServerError__
    , unreadableResult: const $ internalServerError__
    , notFound: const $ notFound__
    , invalidView: const $ internalServerError__
    }

successResponse :: View -> Response
successResponse { administratorId, name, handle, description } = ok_ $ writeJSON
    { administratorId: toInt administratorId
    , name: unwrap name
    , handle: unwrap handle
    , description: unwrap description
    }

response :: Async ViewError View -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
