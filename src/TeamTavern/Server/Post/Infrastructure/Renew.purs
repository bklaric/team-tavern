module TeamTavern.Server.Post.Infrastructure.Renew (renew) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), QueryParameter)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)
import TeamTavern.Server.Post.Infrastructure.ClearExpiry (clearExpiry)
import TeamTavern.Server.Post.Infrastructure.NotifyFits (notifyFits)

-- Whether the post had expired is read before the update, since returning sees
-- only the new time.
renewQuery :: String -> Query
renewQuery condition = Query $ """
    update post
    set updated = now()
    from (
        select post.id, post.updated <= now() - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end as expired
        from post
        join game on game.id = post.game_id
        where """ <> condition <> """
        for update of post
    ) renewed
    where post.id = renewed.id
    returning post.id, renewed.expired
    """

-- | Renews the post the condition picks, over a `post` and its `game`, and
-- | answers its id. Renewing an expired post tells the owners of the posts it
-- | fits again, as publishing it did (brief 8); renewing an active one tells
-- | nobody.
renew :: ∀ querier errors. Querier querier =>
    querier -> String -> Array QueryParameter -> Async (LoadSingleError errors) Int
renew querier condition parameters = do
    { id, expired } :: { id :: Int, expired :: Boolean } <-
        queryFirstNotFound querier (renewQuery condition) parameters
    clearExpiry querier id
    when expired $ notifyFits querier id
    pure id
