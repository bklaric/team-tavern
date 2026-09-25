module TeamTavern.Server.Post.ViewPost (viewPost) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Foreign (Foreign)
import Jarilo (internal__, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Post.ViewPost as ViewPost
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.OwnPost (OwnerView)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Error (Terror(..), elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.CardColumns (cardColumns)
import TeamTavern.Server.Post.Infrastructure.OwnerColumns (ownerView)
import Yoga.JSON.Async (read)

-- The page shows no marks: a description is the viewer's own, and what fits is
-- the feed's to say (brief 11.1). A block either way still shows the post.
postQuery :: Query
postQuery = Query $ """
    with parameters as (
        select $1::text as handle, $2::integer as id, $3::integer as viewer, now() as now
    )
    select
        """ <> cardColumns <> """,
        '{}'::jsonb as marks,
        case
            when exists (select from block
                where blocker_id = parameters.viewer and blocked_id = post.player_id)
            then 'viewer'
            when exists (select from block
                where blocker_id = post.player_id and blocked_id = parameters.viewer)
            then 'owner'
        end as blocked,
        case when post.player_id = parameters.viewer then """ <> ownerView <> """ end as owner_view
    from parameters
    join game on game.handle = parameters.handle
    join post on post.game_id = game.id and post.id = parameters.id
    join player owner on owner.id = post.player_id
    """

-- Six of the game's active posts: those after this one in the order of a feed
-- nobody has described, the most recently active first, and then round from the
-- top. Every active post is so linked from the one before it, and a crawler
-- that reaches any of them reaches them all.
moreQuery :: Query
moreQuery = Query $ """
    with parameters as (
        select $1::text as handle, $2::integer as id, $3::integer as viewer, now() as now
    ),
    this_post as (
        select post.game_id, post.updated, post.id
        from parameters
        join game on game.handle = parameters.handle
        join post on post.game_id = game.id and post.id = parameters.id
    )
    select
        """ <> cardColumns <> """,
        '{}'::jsonb as marks
    from parameters
    cross join this_post
    join post on post.game_id = this_post.game_id and post.id <> this_post.id
    join player owner on owner.id = post.player_id
    where post.updated > parameters.now - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end
        and (parameters.viewer is null or not exists (
            select from block
            where blocker_id = parameters.viewer and blocked_id = post.player_id
                or blocker_id = post.player_id and blocked_id = parameters.viewer))
    order by
        (post.updated, post.id) < (this_post.updated, this_post.id) desc,
        post.updated desc,
        post.id desc
    limit 6
    """

-- The columns the query adds to the card's.
type Extras =
    { blocked :: Maybe String
    , owner_view :: Maybe OwnerView
    }

viewPost :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
viewPost pool handle id cookies =
    sendResponse "Error viewing post" do
    viewer <- checkSignedIn pool cookies <#> map (_.id >>> unwrap)
    row :: Foreign <- queryFirstNotFound pool postQuery (handle : id :| toNullable viewer)
        # lmap (elaborate ("Can't find post " <> show id <> " of game " <> handle))
    post :: CardRow <- read row # lmap readError
    extras :: Extras <- read row # lmap readError
    more :: Array CardRow <- queryMany pool moreQuery (handle : id :| toNullable viewer)
    pure $ ok_ ({ post, blocked: extras.blocked, owner: extras.owner_view, more } :: ViewPost.OkContent)
    where
    readError errors = Terror internal__ [ "Error reading post row: " <> show errors ]
