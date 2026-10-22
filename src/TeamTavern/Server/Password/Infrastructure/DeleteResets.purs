module TeamTavern.Server.Password.Infrastructure.DeleteResets (deleteResets) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

queryString :: Query
queryString = Query """
    delete from password_reset where player_id = $1
    """

-- | Ends every reset link of the player's, once their password is set or their
-- | address changes. Another link asked for before, perhaps by whoever the new
-- | password locks out, would otherwise set it again.
deleteResets :: ∀ querier errors. Querier querier =>
    querier -> Int -> Async (InternalTerror_ errors) Unit
deleteResets querier playerId = queryNone querier queryString (playerId : [])
