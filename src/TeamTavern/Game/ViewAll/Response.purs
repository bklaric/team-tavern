module TeamTavern.Game.ViewAll.Response (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Game.Domain.Types (ViewAll)
import TeamTavern.Game.ViewAll.Types (ViewAllError)
import TeamTavern.Player.Domain.PlayerId (toInt)

type OkContent = Array
    { administratorId :: Int
    , title :: String
    , handle :: String
    , description :: String
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { unreadableViews: const $ internalServerError__
    , invalidViews: const $ internalServerError__
    , databaseError: const $ internalServerError__
    }

successResponse :: Array ViewAll -> Response
successResponse views  = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped views \{ administratorId, title, handle, description } ->
        { administratorId: toInt administratorId
        , title: unwrap title
        , handle: unwrap handle
        , description: unwrap description
        }

response ::
    Async ViewAllError (Array ViewAll) -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
