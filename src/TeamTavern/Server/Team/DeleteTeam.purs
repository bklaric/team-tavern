module TeamTavern.Server.Team.DeleteTeam (deleteTeam) where

import Prelude

import Async (Async, left)
import Data.Maybe (fromMaybe)
import Jarilo (noContent_, notFound__)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query "delete from team where id = $1"

deleteTeam :: ∀ left. Pool -> Cookies -> { handle :: String } -> Async left _
deleteTeam pool cookies { handle } =
    sendResponse "Error deleting team" do
    cookieInfo <- ensureSignedInOwner pool cookies handle
    result <- queryInternal pool queryString (cookieInfo.teamId : [])
    if fromMaybe 0 (rowCount result) > 0
        then pure $ noContent_
        else left $ Terror notFound__
            [ "No team deleted."
            , "Cookie info: " <> show cookieInfo
            , "Path params: " <> show { handle }
            ]
