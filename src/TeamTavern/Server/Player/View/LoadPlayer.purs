module TeamTavern.Server.Player.View.LoadPlayer (loadPlayer) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, playerAdjustedWeekdayFrom, playerAdjustedWeekdayTo, playerAdjustedWeekendFrom, playerAdjustedWeekendTo, queryFirstNotFound)

queryString :: String -> Query
queryString timezone = Query $ """
    select
        case when $2 then player.email end as email,
        case when $2 then player.password_hash is not null end as "hasPassword",
        player.nickname,
        player.discord_tag as "discordTag",
        player.steam_id as "steamId",
        player.riot_id as "riotId",
        player.battle_tag as "battleTag",
        player.ea_id as "eaId",
        player.ubisoft_username as "ubisoftUsername",
        player.psn_id as "psnId",
        player.gamer_tag as "gamerTag",
        player.friend_code as "friendCode",
        case when $2 then to_char(player.birthday, 'yyyy-mm-dd') end as birthday,
        extract(year from age(player.birthday))::int as age,
        player.location,
        player.languages,
        player.microphone,
        player.timezone,
        case
            when player.weekday_from is not null and player.weekday_to is not null
            then json_build_object(
                'clientFrom', to_char(""" <> playerAdjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                'clientTo', to_char(""" <> playerAdjustedWeekdayTo timezone <> """, 'HH24:MI'),
                'sourceFrom', to_char(player.weekday_from, 'HH24:MI'),
                'sourceTo', to_char(player.weekday_to, 'HH24:MI')
            )
        end as "weekdayOnline",
        case
            when player.weekend_from is not null and player.weekend_to is not null
            then json_build_object(
                'clientFrom', to_char(""" <> playerAdjustedWeekendFrom timezone <> """, 'HH24:MI'),
                'clientTo', to_char(""" <> playerAdjustedWeekendTo timezone <> """, 'HH24:MI'),
                'sourceFrom', to_char(player.weekend_from, 'HH24:MI'),
                'sourceTo', to_char(player.weekend_to, 'HH24:MI')
            )
        end as "weekendOnline",
        coalesce(player_profiles.profiles, '[]') as profiles,
        coalesce(player_teams.teams, '[]') as teams
    from player
        left join (
            select
                profile.player_id,
                coalesce(
                    json_agg(
                        json_build_object(
                            'handle', game.handle,
                            'title', game.title,
                            'platforms', json_build_object(
                                'head', game.platforms[1],
                                'tail', game.platforms[2:]
                            ),
                            'fields', coalesce(fields.fields, '[]'),
                            'platform', profile.platform,
                            'fieldValues', coalesce(field_values.field_values, '[]'),
                            'newOrReturning', profile.new_or_returning,
                            'about', profile.about,
                            'ambitions', profile.ambitions,
                            'updated', profile.updated::text,
                            'updatedSeconds', extract(epoch from (now() - updated))::int
                        )
                        order by profile.updated desc
                    )
                    filter (where profile.player_id is not null),
                    '[]'
                ) as profiles
            from player_profile as profile
                join player on player.id = profile.player_id
                join game on game.id = profile.game_id
                left join (
                    select
                        game_id,
                        coalesce(
                            json_agg(
                                json_build_object(
                                    'ilk', ilk,
                                    'label', label,
                                    'key', key,
                                    'icon', icon,
                                    'domain', domain,
                                    'options', options
                                )
                                order by ordinal
                            )
                            filter (where key is not null),
                            '[]'
                        )
                        as "fields"
                    from (
                        select
                            field.id,
                            field.game_id,
                            field.ilk,
                            field.label,
                            field.key,
                            field.icon,
                            field.ordinal,
                            field.domain,
                            json_agg(
                                json_build_object(
                                    'key', field_option.key,
                                    'label', field_option.label
                                )
                                order by field_option.ordinal
                            )
                            filter (where field_option.id is not null)
                            as options
                        from field
                            left join field_option on field_option.field_id = field.id
                        group by
                            field.id
                        ) as field
                    group by game_id
                ) as fields on fields.game_id = game.id
                left join (
                    select
                        profile.id as profile_id,
                        coalesce(
                            json_agg(json_build_object(
                                'fieldKey', profile_value.key,
                                case
                                    when profile_value.ilk = 2 then 'optionKey'
                                    when profile_value.ilk = 3 then 'optionKeys'
                                end,
                                case
                                    when profile_value.ilk = 2 then single
                                    when profile_value.ilk = 3 then multi
                                end
                            )) filter (where profile_value.field_value_id is not null),
                            '[]'
                        ) as field_values
                    from (
                        select
                            field_value.player_profile_id,
                            field.key,
                            field.ilk,
                            field_value.id as field_value_id,
                            to_json(single.key) as single,
                            json_agg(multi.key) as multi
                        from
                            player_profile_field_value as field_value
                        join field
                            on field.id = field_value.field_id
                        left join player_profile_field_value_option as field_value_option
                            on field_value_option.player_profile_field_value_id = field_value.id
                        left join field_option as single
                            on single.id = field_value_option.field_option_id
                            and field.ilk = 2
                        left join field_option as multi
                            on multi.id = field_value_option.field_option_id
                            and field.ilk = 3
                        group by
                            field.id,
                            field_value.id,
                            single.id
                    ) as profile_value
                    join player_profile as profile on profile.id = profile_value.player_profile_id
                    group by profile.id
                ) as field_values on field_values.profile_id = profile.id
            where lower(player.nickname) = lower($1)
            group by profile.player_id
        ) as player_profiles on player_profiles.player_id = player.id
        left join (
            select
                team.owner_id,
                coalesce(
                    json_agg(
                        json_build_object(
                            'handle', team.handle,
                            'organization',
                                case
                                    when team.organization = 'informal'
                                    then json_build_object(
                                        'type', '"informal"'::jsonb,
                                        'value', '{}'::jsonb
                                    )
                                    when team.organization = 'organized'
                                    then json_build_object(
                                        'type', '"organized"'::jsonb,
                                        'value', json_build_object(
                                            'name', team.name
                                        )
                                    )
                                end,
                            'updated', team.updated::text,
                            'updatedSeconds', extract(epoch from (now() - team.updated))::int
                        )
                        order by team.updated desc
                    )
                    filter (where team.owner_id is not null),
                    '[]'
                ) as teams
            from team
            group by team.owner_id
        ) as player_teams on player_teams.owner_id = player.id
    where lower(player.nickname) = lower($1)
    """

loadPlayer
    :: ∀ errors querier
    .  Querier querier
    => querier
    -> Maybe CookieInfo
    -> ViewPlayer.RouteParams
    -> Async (LoadSingleError errors) ViewPlayer.OkContent
loadPlayer pool cookieInfo { nickname, timezone } = do
    let samePlayer = maybe false ((_.nickname) >>> unwrap >>> (_ == nickname)) cookieInfo
    queryFirstNotFound pool (queryString timezone) (nickname :| samePlayer)
        # lmap (elaborate ("Can't find player: " <> nickname))
