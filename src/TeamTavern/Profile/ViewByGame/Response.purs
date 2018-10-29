module TeamTavern.Profile.ViewByGame.Response (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.Domain.Types (ByGameView)
import TeamTavern.Profile.ViewByGame.Types (ViewByGameError)

type OkContent = Array
    { nickname :: String
    , summary :: String
    }

errorResponse :: ViewByGameError -> Response
errorResponse = match
    { invalidHandle: const notFound__
    , databaseError: const internalServerError__
    , unreadableViews: const internalServerError__
    , invalidViews: const internalServerError__
    }

successResponse :: Array ByGameView -> Response
successResponse profiles = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped profiles \{ nickname, summary } ->
        { nickname: unwrap nickname
        , summary: unwrap summary
        }

response
    :: Async ViewByGameError (Array ByGameView)
    -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
