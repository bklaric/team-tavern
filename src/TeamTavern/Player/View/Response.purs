module TeamTavern.Player.View.Response (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.Domain.Types (View)
import TeamTavern.Player.View.Types (ViewError)

type OkContent =
    { nickname :: String
    , about :: String
    , profiles :: Array
        { handle :: String
        , name :: String
        , summary :: String
        }
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { invalidNickname: const $ notFound__
    , databaseError: const $ internalServerError__
    , unreadableResult: const $ internalServerError__
    , notFound: const $ notFound__
    , invalidView: const $ internalServerError__
    }

successResponse :: View -> Response
successResponse { nickname, about, profiles } = ok_ $ writeJSON (
    { nickname: unwrap nickname
    , about: unwrap about
    , profiles: profiles <#> \{ handle, name, summary } ->
        { handle: unwrap handle
        , name: unwrap name
        , summary: unwrap summary
        }
    } :: OkContent)

response :: Async ViewError View -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
