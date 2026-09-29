module TeamTavern.Server.Post.DeletePost (deletePost) where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The post's answers, conversations and notifications go with it.
deleteQuery :: Query
deleteQuery = Query """
    delete from post
    using game
    where game.id = post.game_id and game.handle = $1 and post.player_id = $2 and post.ilk = $3
    returning post.id
    """

deletePost :: ∀ left. Pool -> String -> String -> Cookies -> Async left _
deletePost pool handle type_ cookies =
    sendResponse "Error deleting post" do
    { id } <- ensureSignedIn pool cookies
    (_ :: { id :: Int }) <- queryFirstNotFound pool deleteQuery (handle : id :| type_)
    pure noContent_
