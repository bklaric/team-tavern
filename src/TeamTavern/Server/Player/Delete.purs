module TeamTavern.Server.Player.Delete (delete) where

import Prelude

import Async (Async, left)
import Data.Maybe (fromMaybe)
import Jarilo (noContent, notFound__)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, removeCookieHeader)
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
    if fromMaybe 0 (rowCount result) > 0
        then pure $ noContent removeCookieHeader
        else left $ Terror notFound__
            [ "No player deleted."
            , "Cookie info: " <> show cookieInfo
            , "Path params: " <> show { nickname }
            ]
