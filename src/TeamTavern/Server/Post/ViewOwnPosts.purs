module TeamTavern.Server.Post.ViewOwnPosts (viewOwnPosts) where

import Prelude

import Async (Async)
import Data.Array (groupBy)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign (Foreign)
import Jarilo (internal__, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Post.ViewOwnPosts as ViewOwnPosts
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description)
import TeamTavern.Routes.Shared.OwnPost (OwnerView)
import TeamTavern.Server.Feed.ViewOwnDescriptions (descriptionJson)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.CardColumns (cardColumns)
import TeamTavern.Server.Post.Infrastructure.OwnerColumns (ownerView)
import Yoga.JSON.Async (read)

postsQuery :: Query
postsQuery = Query $ """
    with parameters as (
        select $1::integer as viewer, now() as now
    )
    select
        game.handle,
        """ <> cardColumns <> """,
        '{}'::jsonb as marks,
        """ <> ownerView <> """ as owner_view,
        """ <> descriptionJson <> """ as description
    from parameters
    join post on post.player_id = parameters.viewer
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    order by game.title, array_position(array['player', 'group', 'community'], post.ilk)
    """

-- The columns the query adds to the card's.
type Extras =
    { handle :: String
    , owner_view :: OwnerView
    , description :: Description
    }

viewOwnPosts :: ∀ left. Pool -> Cookies -> Async left _
viewOwnPosts pool cookies =
    sendResponse "Error viewing own posts" do
    { id } <- ensureSignedIn pool cookies
    rows :: Array Foreign <- queryMany pool postsQuery (unwrap id : [])
    posts <- rows # traverse \row -> do
        post :: CardRow <- read row # lmap readError
        extras :: Extras <- read row # lmap readError
        pure { handle: extras.handle, post, owner: extras.owner_view, description: extras.description }
    let games = posts
            # groupBy (\one other -> one.handle == other.handle)
            <#> \game ->
                { handle: (NonEmptyArray.head game).handle
                , posts: NonEmptyArray.toArray game <#> \{ post, owner, description } -> { post, owner, description }
                }
    pure $ ok_ (games :: ViewOwnPosts.OkContent)
    where
    readError errors = Terror internal__ [ "Error reading own post row: " <> show errors ]
