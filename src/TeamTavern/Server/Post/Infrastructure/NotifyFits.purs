module TeamTavern.Server.Post.Infrastructure.NotifyFits (notifyFits) where

import Prelude

import Async (Async)
import Effect.Class (liftEffect)
import JavaScript.Date as Date
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:|))
import TeamTavern.Server.Feed.Fits (fitsText)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

-- A post that fits again, renewed after it expired, brings its notification
-- back to the top unread rather than adding a second (notification_fit_key).
notifyQuery :: Query
notifyQuery = Query $ """
    insert into notification (post_id, kind, fitting_post_id, created)
    select fits.id, 'fit', $1, $2
    from (""" <> fitsText <> """) fits
    on conflict (post_id, fitting_post_id) where kind = 'fit'
    do update set created = excluded.created, read = false
    """

-- | Tells the owners of the posts a post fits that it does (brief 8), when it
-- | is published or renewed after it expired.
notifyFits :: ∀ querier errors. Querier querier => querier -> Int -> Async (InternalTerror_ errors) Unit
notifyFits querier postId = do
    now <- liftEffect Date.now
    queryNone querier notifyQuery (postId :| now)
