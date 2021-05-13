module TeamTavern.Server.Alert.Delete where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect, left)
import Data.Symbol (SProxy(..))
import Data.Variant (inj, match)
import Effect (Effect)
import Perun.Response (Response, internalServerError__, noContent_, notFound__)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Routes.DeleteAlert (RouteContent)
import TeamTavern.Server.Infrastructure.Error (InternalError, NotFoundRow)
import TeamTavern.Server.Infrastructure.Log (internalHandler, notFoundHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)

type DeleteAlertError = (InternalError (NotFoundRow ()))

logError :: DeleteAlertError -> Effect Unit
logError = Log.logError "Error deleting alert"
    (   internalHandler
    >>> notFoundHandler
    )

errorResponse :: DeleteAlertError -> Response
errorResponse = match
    { internal: const internalServerError__
    , notFound: const notFound__
    }

sendResponse :: Async DeleteAlertError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const noContent_)

queryString :: Query
queryString = Query "delete from alert where id = $1 and token = $2;"

deleteAlert :: forall left. Pool -> RouteContent -> Async left Response
deleteAlert pool { id, token } =
    sendResponse $ examineLeftWithEffect logError do
    result <- queryInternal pool queryString (id :| token)
    if rowCount result > 0
        then pure unit
        else left $ inj (SProxy :: _ "notFound")
            [ "No alert deleted for id=(" <> show id <> ") token=(" <> token <> ")." ]
