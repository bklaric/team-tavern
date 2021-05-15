module TeamTavern.Server.Player.Delete where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect, left)
import Data.Symbol (SProxy(..))
import Data.Variant (inj, match)
import Effect (Effect)
import Perun.Response (Response, forbidden__, internalServerError__, noContent, notFound__, unauthorized__)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, removeCookieHeader)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Error (InternalError, NotAuthorizedRow, NotFoundRow, NotAuthenticatedRow)
import TeamTavern.Server.Infrastructure.Log (internalHandler, notAuthenticatedHandler, notAuthorizedHandler, notFoundHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)

type DeletePlayerError =
    (InternalError (NotFoundRow (NotAuthenticatedRow (NotAuthorizedRow ()))))

logError :: DeletePlayerError -> Effect Unit
logError = Log.logError "Error deleting player"
    (   internalHandler
    >>> notFoundHandler
    >>> notAuthenticatedHandler
    >>> notAuthorizedHandler
    )

errorResponse :: DeletePlayerError -> Response
errorResponse = match
    { internal: const internalServerError__
    , notFound: const notFound__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    }

sendResponse :: Async DeletePlayerError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const $ noContent removeCookieHeader)

queryString :: Query
queryString = Query "delete from player where id = $1;"

delete :: forall left. Pool -> String -> Cookies -> Async left Response
delete pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- ensureSignedInAs pool cookies nickname
    result <- queryInternal pool queryString (cookieInfo.id : [])
    if rowCount result > 0
        then pure unit
        else left $ inj (SProxy :: _ "notFound")
            [ "No player deleted for id=(" <> show cookieInfo.id <> ")." ]
