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
    , title :: String
    , handle :: String
    , description :: String
    , hasProfile :: Boolean
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
successResponse { administratorId, title, handle, description, hasProfile } =
    ok_ $ writeJSON (
    { administratorId: toInt administratorId
    , title: unwrap title
    , handle: unwrap handle
    , description: unwrap description
    , hasProfile
    } :: OkContent)

response :: Async ViewError View -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
