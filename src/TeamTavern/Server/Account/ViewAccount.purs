module TeamTavern.Server.Account.ViewAccount (viewAccount) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Account.ViewAccount as ViewAccount
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.ContactAccount (contactAccount)

-- The contact kinds in the order the page asks for them: Discord, which every
-- game offers, then the game accounts, then the consoles'.
accountQuery :: Query
accountQuery = Query $ """
    select
        player.nickname,
        to_char(player.birthday, 'YYYY-MM-DD') as birthday,
        player.country,
        player.languages,
        player.timezone,
        coalesce((
            select jsonb_agg(jsonb_build_object(
                    'kind', offered.kind,
                    'value', """ <> contactAccount "player" "offered.kind" <> """,
                    'games', offered.games,
                    'everyGame', offered.every_game
                ) order by array_position(
                    array['discord', 'riot', 'battle_tag', 'ea', 'ubisoft', 'steam', 'psn', 'gamer_tag', 'friend_code'],
                    offered.kind))
            from (
                select
                    game_contact.kind,
                    array_agg(game.title order by game.title) as games,
                    count(*) = (select count(*) from game) as every_game
                from game_contact
                join game on game.id = game_contact.game_id
                group by game_contact.kind
            ) as offered
        ), '[]') as contacts,
        player.email,
        player.email_confirmed as "emailConfirmed",
        case when player.discord_id is null then 'password' else 'discord' end as "signIn",
        jsonb_build_object(
            'matches', player.email_matches,
            'messages', player.email_messages,
            'renewals', player.email_renewals
        ) as switches,
        (select count(*)::int from post where post.player_id = player.id) as posts,
        (
            select count(*)::int
            from conversation
            join post on post.id = conversation.post_id
            where post.player_id = player.id or conversation.messager_id = player.id
        ) as conversations
    from player
    where player.id = $1
    """

viewAccount :: ∀ left. Pool -> Cookies -> Async left _
viewAccount pool cookies =
    sendResponse "Error viewing account" do
    { id } <- ensureSignedIn pool cookies
    account :: ViewAccount.OkContent <- queryFirstInternal pool accountQuery (unwrap id : [])
    pure $ ok_ account
