module TeamTavern.Server.Block.Block (block) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The viewer never finds themselves, so `block_not_self` can't fire.
blockQuery :: Query
blockQuery = Query """
    with target as (
        select id from player where lower(nickname) = lower($2) and id <> $1
    ),
    added as (
        insert into block (blocker_id, blocked_id)
        select $1, id from target
        on conflict do nothing
    )
    select id from target
    """

block :: ∀ left. Pool -> String -> Cookies -> Async left _
block pool nickname cookies =
    sendResponse "Error blocking player" do
    { id } <- ensureSignedIn pool cookies
    (_ :: { id :: Int }) <- queryFirstNotFound pool blockQuery (unwrap id :| nickname)
        # lmap (elaborate ("Can't find player " <> nickname <> " to block"))
    pure noContent_
