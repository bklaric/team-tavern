module TeamTavern.Server.Game.ViewAll where

import Prelude

import Async (Async)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

loadGamesQuery :: Query
loadGamesQuery = Query """
    select
        game.title,
        game.short_title as "shortTitle",
        game.handle,
        game.description
    from game
    order by game.created
    """

loadGames :: forall errors. Pool -> Async (InternalError_ errors) ViewAllGames.OkContent
loadGames pool = queryMany_ pool loadGamesQuery

viewAll :: forall left. Pool -> Async left _
viewAll pool =
    sendResponse "Error viewing all games" do
    -- Load games from database
    ok_ <$> loadGames pool
