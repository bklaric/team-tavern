module TeamTavern.Server.Player.Delete where

import Prelude

import Async (Async, left)
import Jarilo (noContent_, notFound__)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query "delete from player where id = $1"

delete :: ∀ left. Pool -> String -> Cookies -> Async left _
delete pool nickname cookies =
    sendResponse "Error deleting player" do
    cookieInfo <- ensureSignedInAs pool cookies nickname
    result <- queryInternal pool queryString (cookieInfo.id : [])
    if rowCount result > 0
        then pure noContent_
        else left $ Terror notFound__
            [ "No player deleted for id=(" <> show cookieInfo.id <> ")." ]
