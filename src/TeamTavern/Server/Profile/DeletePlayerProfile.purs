module TeamTavern.Server.Profile.DeletePlayerProfile (deletePlayerProfile) where

import Prelude

import Async (Async, left)
import Data.Maybe (fromMaybe)
import Jarilo (noContent_, notFound__)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Routes.Profile.DeletePlayerProfile as DPP
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query """
    delete from player_profile
    using game
    where game.id = player_profile.game_id
    and player_profile.player_id = $1
    and game.handle = $2
    """

deletePlayerProfile :: ∀ left. Pool -> Cookies -> DPP.PathParams -> Async left _
deletePlayerProfile pool cookies { nickname, handle } =
    sendResponse "Error deleting player profile" do
    cookieInfo <- ensureSignedInAs pool cookies nickname
    result <- queryInternal pool queryString (cookieInfo.id :| handle)
    if fromMaybe 0 (rowCount result) > 0
        then pure noContent_
        else left $ Terror notFound__
            [ "No player profile deleted."
            , "Cookie info: " <> show cookieInfo
            , "Path params: " <> show { nickname, handle }
            ]
