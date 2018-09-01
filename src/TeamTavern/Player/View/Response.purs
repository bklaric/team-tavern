module TeamTavern.Player.View.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Domain.Types (NicknamedAbout)
import TeamTavern.Player.View.Types (ViewError)

type OkResponseContent =
    { nickname :: String
    , about :: String
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { nicknameInvalid: const $ notFound__
    , databaseError: const $ internalServerError__
    , unreadableResult: const $ internalServerError__
    , notFound: const $ notFound__
    , invalidView: const $ internalServerError__
    }

successResponse :: NicknamedAbout -> Response
successResponse { nickname, about } = ok_ $ writeJSON
    { nickname: unwrap nickname, about: unwrap about }

response :: Async ViewError NicknamedAbout -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
