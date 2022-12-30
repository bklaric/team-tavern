module TeamTavern.Server.Profile.DeleteTeamProfile (deleteTeamProfile) where

import Prelude

import Async (Async, left)
import Jarilo (noContent_, notFound__)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (rowCount)
import TeamTavern.Routes.Profile.DeleteTeamProfile as DTP
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query """
    delete from team_profile
    using game
    where game.id = team_profile.game_id
    and team_profile.team_id = $1
    and game.handle = $2
    """

deleteTeamProfile :: ∀ left. Pool -> Cookies -> DTP.PathParams -> Async left _
deleteTeamProfile pool cookies { teamHandle, gameHandle } =
    sendResponse "Error deleting team profile" do
    cookieInfo <- ensureSignedInOwner pool cookies teamHandle
    result <- queryInternal pool queryString (cookieInfo.teamId :| gameHandle)
    if rowCount result > 0
        then pure noContent_
        else left $ Terror notFound__
            [ "No team profile deleted."
            , "Cookie info: " <> show cookieInfo
            , "Path params: " <> show { teamHandle, gameHandle }
            ]
