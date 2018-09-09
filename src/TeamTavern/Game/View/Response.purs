module TeamTavern.Game.View.Response (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Game.View.Types (ViewError)
import TeamTavern.Player.Domain.PlayerId (toInt)

type OkContent =
    { administratorId :: Int
    , name :: String
    , handle :: String
    , description :: String
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { invalidHandle: const $ notFound__
    , databaseError: const $ internalServerError__
    , unreadableView: const $ internalServerError__
    , notFound: const $ notFound__
    , invalidView: const $ internalServerError__
    }

successResponse :: View -> Response
successResponse { administratorId, name, handle, description } =
    ok_ $ writeJSON (
    { administratorId: toInt administratorId
    , name: unwrap name
    , handle: unwrap handle
    , description: unwrap description
    } :: OkContent)

response :: Async ViewError View -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
