module TeamTavern.Server.Post.Infrastructure.CardColumns (cardColumns) where

import Prelude

import TeamTavern.Server.Post.Infrastructure.Answers (flagsJson, optionsJson, rangesJson)

-- | A card's columns but its marks, as `Feed.sql`'s last select writes them,
-- | since both decode into `CardRow`: SQL over a `post`, its `owner` and
-- | `parameters` with the `viewer` (null signed out) and `now` in scope.
cardColumns :: String
cardColumns = """
    post.id,
    post.ilk as type,
    post.name,
    owner.nickname as owner,
    coalesce(post.player_id = parameters.viewer, false) as own,
    (
        select to_jsonb(min(message.created))
        from conversation
        join message on message.conversation_id = conversation.id
        where conversation.post_id = post.id and conversation.messager_id = parameters.viewer
            and message.sender_id = parameters.viewer
    ) as messaged,
    to_jsonb(post.updated) as updated,
    post.updated <= parameters.now - case when post.ilk = 'community'
        then interval '90 days' else interval '30 days' end as expired,
    post.summary,
    case when post.ilk = 'player' then date_part('year', age(parameters.now, owner.birthday)) end as age,
    case when post.ilk = 'player' then owner.country end as country,
    case when post.ilk = 'player' then owner.languages else post.languages end as languages,
    post.regions,
    post.age_from,
    post.age_to,
    post.group_size,
    post.group_wanted_from,
    post.group_wanted_to,
    owner.timezone,
    to_char(post.online_from, 'HH24:MI') as online_from,
    to_char(post.online_to, 'HH24:MI') as online_to,
    post.microphone,
    post.contact_preference,
    array(
        select kind from game_contact
        where game_contact.game_id = post.game_id
            and post.ilk <> 'community'
            and case kind
                when 'discord' then owner.discord_tag
                when 'steam' then owner.steam_id
                when 'riot' then owner.riot_id
                when 'battle_tag' then owner.battle_tag
                when 'ea' then owner.ea_id
                when 'ubisoft' then owner.ubisoft_username
                when 'psn' then owner.psn_id
                when 'gamer_tag' then owner.gamer_tag
                when 'friend_code' then owner.friend_code
            end is not null
        order by kind
    ) as contacts,
    coalesce((
        select jsonb_agg(jsonb_build_object(
            'title', tracker.title, 'template', tracker.template, 'account', account
        ) order by tracker.id)
        from tracker
        cross join lateral (select case tracker.contact_kind
            when 'discord' then owner.discord_tag
            when 'steam' then owner.steam_id
            when 'riot' then owner.riot_id
            when 'battle_tag' then owner.battle_tag
            when 'ea' then owner.ea_id
            when 'ubisoft' then owner.ubisoft_username
            when 'psn' then owner.psn_id
            when 'gamer_tag' then owner.gamer_tag
            when 'friend_code' then owner.friend_code
        end as account) accounts
        where tracker.game_id = post.game_id
            and post.ilk = 'player'
            and account is not null
    ), '[]') as trackers,
    post.discord_server is not null as has_discord_server,
    post.website is not null as has_website,
    """ <> optionsJson <> """ as options,
    """ <> rangesJson <> """ as ranges,
    """ <> flagsJson <> """ as flags
    """
