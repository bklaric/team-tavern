module TeamTavern.Server.Post.RenewByNonce (renewByNonce) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Post.RenewByNonce as RenewByNonce
import TeamTavern.Server.Feed.ViewOwnDescriptions (descriptionJson)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.Renew (renew)

renewedQuery :: Query
renewedQuery = Query $ """
    select
        game.handle,
        post.id,
        post.ilk as type,
        """ <> descriptionJson <> """ as description
    from post
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    where post.id = $1
    """

-- | The nonce is the post's for as long as the post lives, so every email about
-- | it carries the same link, and a link opened twice renews twice.
renewByNonce :: ∀ left. Pool -> RenewByNonce.RequestContent -> Async left _
renewByNonce pool { nonce } =
    sendResponse "Error renewing post by nonce" do
    renewed :: RenewByNonce.OkContent <- pool # transaction \client -> do
        id <- renew client "post.renewal_nonce = $1" (nonce : [])
            # lmap (elaborate "Can't find a post to renew by nonce")
        queryFirstInternal client renewedQuery (id : [])
    pure $ ok_ renewed
