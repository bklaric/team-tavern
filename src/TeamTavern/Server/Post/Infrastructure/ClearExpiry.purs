module TeamTavern.Server.Post.Infrastructure.ClearExpiry (clearExpiry) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

clearQuery :: Query
clearQuery = Query """
    delete from notification
    where post_id = $1 and kind = 'expiry'
    """

-- | A renewed post is active for its whole term again, so its notice that it
-- | is expiring goes (brief 11.3).
clearExpiry :: ∀ querier errors. Querier querier => querier -> Int -> Async (InternalTerror_ errors) Unit
clearExpiry querier postId = queryNone querier clearQuery (postId : [])
