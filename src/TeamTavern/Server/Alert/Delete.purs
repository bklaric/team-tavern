module TeamTavern.Server.Alert.Delete where

import Prelude

import Async (Async, left)
import Data.Maybe (fromMaybe)
import Jarilo (noContent_, notFound__)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Routes.Alert.DeleteAlert (RouteContent)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query "delete from alert where id = $1 and token = $2;"

deleteAlert :: ∀ left. Pool -> RouteContent -> Async left _
deleteAlert pool { id, token } =
    sendResponse "Error deleting alert" do
    result <- queryInternal pool queryString (id :| token)
    if fromMaybe 0 (rowCount result) > 0
        then pure noContent_
        else left $ Terror notFound__
            [ "No alert deleted for id=(" <> show id <> ") token=(" <> token <> ")." ]
