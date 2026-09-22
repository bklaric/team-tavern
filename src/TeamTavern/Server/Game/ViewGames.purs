module TeamTavern.Server.Game.ViewGames (viewGames) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Routes.Game.ViewGames as ViewGames
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The catalogue's order is the titles' order, wherever the site lists games.
loadGamesQuery :: Query
loadGamesQuery = Query """
    select game.handle, game.title
    from game
    order by game.title
    """

loadGames :: ∀ errors. Pool -> Async (InternalTerror_ errors) ViewGames.OkContent
loadGames pool = queryMany_ pool loadGamesQuery

viewGames :: ∀ left. Pool -> Async left _
viewGames pool =
    sendResponse "Error viewing games" do
    ok_ <$> loadGames pool
