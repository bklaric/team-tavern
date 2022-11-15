module TeamTavern.Server.Game.ViewAll.LoadGames (loadGames) where

import Async (Async)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)

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

loadGames :: forall errors. Pool -> Async (InternalError errors) ViewAllGames.OkContent
loadGames pool = queryMany_ pool loadGamesQuery
