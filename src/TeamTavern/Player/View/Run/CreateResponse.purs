module TeamTavern.Player.View.Run.CreateResponse
    (OkResponseContent, viewResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Domain.PlayerId (toInt)
import TeamTavern.Player.View (PlayerView)
import TeamTavern.Player.View.Run.Types (ViewError)

type OkResponseContent =
    { id :: Int
    , nickname :: String
    , about :: String
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { readNickname: const $ badRequest__
    , loadPlayer: const $ internalServerError__
    }

successResponse :: PlayerView -> Response
successResponse { id, nickname, about } = ok_ $ writeJSON
    ({ id: toInt id, nickname: unwrap nickname, about: unwrap about }
    :: OkResponseContent)

viewResponse :: Async ViewError PlayerView -> (forall left. Async left Response)
viewResponse = alwaysRight errorResponse successResponse
