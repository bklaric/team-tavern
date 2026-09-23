module TeamTavern.Server.Post.RevealContacts (revealContacts) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Post.RevealContacts as RevealContacts
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.ContactAccount (contactAccount)

-- A reveal counts only where it shows something, and never the owner's own.
revealQuery :: Query
revealQuery = Query $ """
    with revealed as (
        select
            post.id,
            post.player_id,
            coalesce((
                select jsonb_agg(jsonb_build_object('kind', game_contact.kind, 'value', account)
                    order by game_contact.kind <> 'discord', game_contact.kind)
                from game_contact
                cross join lateral (select """ <> contactAccount "owner" "game_contact.kind" <> """ as account) accounts
                where game_contact.game_id = post.game_id
                    and post.ilk <> 'community'
                    and account is not null
            ), '[]') as contacts,
            post.discord_server,
            post.website
        from game
        join post on post.game_id = game.id
        join player owner on owner.id = post.player_id
        where game.handle = $1 and post.id = $2
            and not exists (
                select from block
                where blocker_id = $3 and blocked_id = post.player_id
                    or blocker_id = post.player_id and blocked_id = $3)
    ),
    counted as (
        update post
        set contact_reveals = contact_reveals + 1
        from revealed
        where post.id = revealed.id
            and revealed.player_id <> $3
            and (revealed.contacts <> '[]'
                or revealed.discord_server is not null
                or revealed.website is not null)
    )
    select contacts, discord_server, website from revealed
    """

revealContacts :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
revealContacts pool handle postId cookies =
    sendResponse "Error revealing contacts" do
    { id } <- ensureSignedIn pool cookies
    content :: RevealContacts.OkContent <- queryFirstNotFound pool revealQuery (handle : postId :| unwrap id)
        # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to reveal"))
    pure $ ok_ content
