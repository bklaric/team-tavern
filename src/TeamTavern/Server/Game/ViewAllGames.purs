module TeamTavern.Server.Game.ViewAllGames (viewAllGames) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
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

loadGames :: ∀ errors. Pool -> Async (InternalTerror_ errors) ViewAllGames.OkContent
loadGames pool = queryMany_ pool loadGamesQuery

viewAllGames :: ∀ left. Pool -> Async left _
viewAllGames pool =
    sendResponse "Error viewing all games" do
    -- Load games from database
    ok_ <$> loadGames pool
