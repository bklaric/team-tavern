module TeamTavern.Server.Worker.AddExpiries (addExpiries) where

import Prelude

import Async (Async)
import JavaScript.Date (Date)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

-- A post's row is added once, as its last week begins, and never refreshed: the
-- period's email takes the rows created in the period, so a refreshed row would
-- be emailed again every period. Renewing the post deletes it, and its next
-- last week adds another.
addQuery :: Query
addQuery = Query """
    insert into notification (post_id, kind, created)
    select post.id, 'expiry', $1::timestamptz
    from post
    where post.updated + case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end
            - interval '7 days' <= $1::timestamptz
        and post.updated > $1::timestamptz - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end
    on conflict (post_id) where kind = 'expiry' do nothing
    """

-- | Tells the owner of every active post in its last week that it is expiring
-- | (brief 11.3), stamped `now` so the period's email finds it.
addExpiries :: ∀ errors. Pool -> Date -> Async (InternalTerror_ errors) Unit
addExpiries pool now = queryNone pool addQuery (now : [])
