module TeamTavern.Server.Post.ViewOwnPost (viewOwnPost) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Post.ViewOwnPost as ViewOwnPost
import TeamTavern.Routes.Shared.Post (AccountContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryFirstMaybe)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.Answers (flagsJson, optionsJson, rangesJson)

accountQuery :: Query
accountQuery = Query """
    select
        player.country,
        player.languages,
        to_char(player.birthday, 'YYYY-MM-DD') as birthday,
        player.timezone,
        jsonb_strip_nulls(jsonb_build_object(
            'discord', player.discord_tag,
            'steam', player.steam_id,
            'riot', player.riot_id,
            'battle_tag', player.battle_tag,
            'ea', player.ea_id,
            'ubisoft', player.ubisoft_username,
            'psn', player.psn_id,
            'gamer_tag', player.gamer_tag,
            'friend_code', player.friend_code
        )) as contacts
    from player
    where player.id = $1
    """

postQuery :: Query
postQuery = Query $ """
    select
        post.id,
        to_jsonb(post.updated) as updated,
        (select count(*)::int from conversation where conversation.post_id = post.id) as conversations,
        jsonb_build_object(
            'options', """ <> optionsJson <> """,
            'ranges', """ <> rangesJson <> """,
            'flags', """ <> flagsJson <> """,
            'name', post.name,
            'groupSize', post.group_size,
            'groupWantedFrom', post.group_wanted_from,
            'groupWantedTo', post.group_wanted_to,
            'regions', to_jsonb(post.regions),
            'languages', to_jsonb(post.languages),
            'ageFrom', post.age_from,
            'ageTo', post.age_to,
            'online', case when post.online_from is not null and post.online_to is not null
                then jsonb_build_object(
                    'from', to_char(post.online_from, 'HH24:MI'),
                    'to', to_char(post.online_to, 'HH24:MI'))
                end,
            'microphone', post.microphone,
            'summary', array_to_string(post.summary, chr(10) || chr(10)),
            'contactPreference', post.contact_preference,
            'discordServer', post.discord_server,
            'website', post.website
        ) as content
    from post
    join game on game.id = post.game_id
    where game.handle = $1 and post.player_id = $2 and post.ilk = $3
    """

viewOwnPost :: ∀ left. Pool -> String -> String -> Cookies -> Async left _
viewOwnPost pool handle type_ cookies =
    sendResponse "Error viewing own post" do
    { id } <- ensureSignedIn pool cookies
    account :: AccountContent <- queryFirstInternal pool accountQuery (id : [])
    post :: Maybe ViewOwnPost.OwnPost <- queryFirstMaybe pool postQuery (handle : id :| type_)
    pure $ ok_ ({ account, post } :: ViewOwnPost.OkContent)
