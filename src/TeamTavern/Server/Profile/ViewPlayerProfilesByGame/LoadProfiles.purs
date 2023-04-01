module TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles
    (createPlayerFilterString, createFieldsJoinString, createFieldsFilterString, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (fold, intercalate, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Postgres.Client (Client)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Profile.Shared (ProfilePage, pageSize)
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame as ViewPlayerProfilesByGame
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Infrastructure.Postgres (adjustedWeekdayFrom, adjustedWeekdayTo, adjustedWeekendFrom, adjustedWeekendTo, prepareString, queryFirstInternal_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Profile.Infrastructure.LoadFieldAndOptionIds (FieldAndOptionIds)

upperAgeLimit :: Int
upperAgeLimit = 200

createAgeFilter :: Maybe Age -> Maybe Age -> String
createAgeFilter Nothing Nothing = ""
createAgeFilter (Just ageFrom) Nothing =
    if ageFrom < upperAgeLimit
    then " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years')"
    else " and false"
createAgeFilter Nothing (Just ageTo) =
    if ageTo < upperAgeLimit
    then " and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"
    else " and true"
createAgeFilter (Just ageFrom) (Just ageTo) =
    if ageFrom < upperAgeLimit && ageTo < upperAgeLimit && ageFrom <= ageTo
    then " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years') "
        <> "and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"
    else " and false"

createLanguagesFilter :: Array Language -> String
createLanguagesFilter [] = ""
createLanguagesFilter languages = " and player.languages && (array[" <> (languages <#> prepareString # intercalate ", ") <> "])"

createCountriesFilter :: Array Location -> String
createCountriesFilter [] = ""
createCountriesFilter locations = """ and player.location in (
    with recursive region_rec(name) as (
        select region.name from region where region.name = any(array[""" <> (locations <#> prepareString # intercalate ", ") <> """])
        union all
        select subregion.name from region as subregion join region_rec on subregion.superregion_name = region_rec.name
    )
    select * from region_rec)
"""

createWeekdayOnlineFilter :: Timezone -> Maybe Time -> Maybe Time -> String
createWeekdayOnlineFilter timezone (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time"
        playerStart = adjustedWeekdayFrom timezone
        playerEnd   = adjustedWeekdayTo timezone
    in
    """ and
    case
        when """ <> playerStart <> """ < """ <> playerEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ and """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ > """ <> playerEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ or """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ < """ <> playerEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ or """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ > """ <> playerEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekdayOnlineFilter _ _ _ = ""

createWeekendOnlineFilter :: Timezone -> Maybe Time -> Maybe Time -> String
createWeekendOnlineFilter timezone (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time"
        playerStart = adjustedWeekendFrom timezone
        playerEnd   = adjustedWeekendTo timezone
    in
    """ and
    case
        when """ <> playerStart <> """ < """ <> playerEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ and """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ > """ <> playerEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ or """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ < """ <> playerEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> playerEnd <> """ or """ <> playerStart <> """ < """ <> toTime <> """
        when """ <> playerStart <> """ > """ <> playerEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekendOnlineFilter _ _ _ = ""

createMicrophoneFilter :: HasMicrophone -> String
createMicrophoneFilter false = ""
createMicrophoneFilter true = " and player.microphone"

createPlatformsFilter :: Array Platform -> String
createPlatformsFilter [] = ""
createPlatformsFilter platforms = " and array[profile.platform] <@ (array[" <> (platforms <#> Platform.toString <#> prepareString # intercalate ", ") <> "])"

createNewOrReturningFilter :: NewOrReturning -> String
createNewOrReturningFilter false = ""
createNewOrReturningFilter true = " and profile.new_or_returning"

createPlayerFilterString :: Timezone -> Filters -> String
createPlayerFilterString timezone filters =
    createAgeFilter filters.ageFrom filters.ageTo
    <> createLanguagesFilter filters.languages
    <> createCountriesFilter filters.locations
    <> createWeekdayOnlineFilter timezone filters.weekdayFrom filters.weekdayTo
    <> createWeekendOnlineFilter timezone filters.weekendFrom filters.weekendTo
    <> createMicrophoneFilter filters.microphone
    <> createPlatformsFilter filters.platforms
    <> createNewOrReturningFilter filters.newOrReturning

createFieldsJoinString :: Array FieldAndOptionIds -> String
createFieldsJoinString fields = fields # mapWithIndex (\index _ -> let
    fieldValue = "field_value_" <> show index
    fieldValueOption = "field_value_option_" <> show index
    in """
    left join player_profile_field_value """ <> fieldValue <> """ on """ <> fieldValue <> """.player_profile_id = profile.id
    left join player_profile_field_value_option """ <> fieldValueOption <> """ on """ <> fieldValueOption <> """.player_profile_field_value_id = """ <> fieldValue <> """.id
    """)
    # fold

createFieldsFilterString :: Array FieldAndOptionIds -> String
createFieldsFilterString fields = fields # mapWithIndex (\index {fieldId, optionIds} -> let
    fieldValue = "field_value_" <> show index
    fieldValueOption = "field_value_option_" <> show index
    in
    " and " <> fieldValue <> ".field_id = " <> show fieldId <> " and ("
        <> ((optionIds <#> \optionId -> fieldValueOption <> ".field_option_id = " <> show optionId) # joinWith " or ")
        <> ")")
    # fold

queryStringFilter :: Handle -> ProfilePage -> Timezone -> Filters -> Array FieldAndOptionIds -> String
queryStringFilter handle page timezone filters fieldAndOptionIds = """
    select profile.id, count(*) over () as total
    from player_profile profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id """
    <> createFieldsJoinString fieldAndOptionIds <> """
    where game.handle = """ <> prepareString handle
    <> createPlayerFilterString timezone filters
    <> createFieldsFilterString fieldAndOptionIds
    <> """
    group by profile.id, profile.updated
    order by profile.updated desc
    limit """ <> show pageSize <> """ offset """
    <> show ((max 0 (page - 1)) * pageSize)

queryString :: Handle -> ProfilePage -> Timezone -> Filters -> Array FieldAndOptionIds -> Query
queryString handle page timezone filters fieldAndOptionIds = Query $ """
select total::int as count, json_agg(json_build_object(
    'nickname', "nickname",
    'discordTag', "discordTag",
    'steamId', "steamId",
    'riotId', "riotId",
    'battleTag', "battleTag",
    'eaId', "eaId",
    'ubisoftUsername', "ubisoftUsername",
    'psnId', "psnId",
    'gamerTag', "gamerTag",
    'friendCode', "friendCode",
    'age', "age",
    'location', "location",
    'languages', "languages",
    'microphone', "microphone",
    'weekdayOnline', "weekdayOnline",
    'weekendOnline', "weekendOnline",
    'platforms', "platforms",
    'platform', "platform",
    'about', "about",
    'ambitions', "ambitions",
    'newOrReturning', "newOrReturning",
    'updated', "updated",
    'updatedSeconds', "updatedSeconds",
    'fieldValues', "fieldValues"
) order by updated desc) as profiles
from (
    select
        nickname,
        "discordTag",
        "steamId",
        "riotId",
        "battleTag",
        "eaId",
        "ubisoftUsername",
        "psnId",
        "gamerTag",
        "friendCode",
        age,
        location,
        languages,
        microphone,
        case
            when weekday_from is not null and weekday_to is not null
            then json_build_object(
                'from', to_char(""" <> adjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                'to', to_char(""" <> adjustedWeekdayTo timezone <> """, 'HH24:MI')
            )
        end as "weekdayOnline",
        case
            when weekend_from is not null and weekend_to is not null
            then json_build_object(
                'from', to_char(""" <> adjustedWeekendFrom timezone <> """, 'HH24:MI'),
                'to', to_char(""" <> adjustedWeekendTo timezone <> """, 'HH24:MI')
            )
        end as "weekendOnline",
        json_build_object(
            'head', platforms[1],
            'tail', platforms[2:]
        ) as platforms,
        platform,
        about,
        ambitions,
        "newOrReturning",
        updated,
        "updatedSeconds",
        total,
        coalesce(
            jsonb_agg(
                jsonb_build_object(
                    'field', jsonb_build_object(
                        'ilk', field_ilk,
                        'key', field_key,
                        'label', field_label,
                        'icon', field_icon
                    ),
                    case
                        when field_ilk = 'single' then 'option'
                        when field_ilk = 'multi' then 'options'
                    end,
                    case
                        when field_ilk = 'single' then field_options->0
                        when field_ilk = 'multi' then field_options
                    end
                ) order by field_ordinal
            ) filter (where field_key is not null),
            '[]'
        ) as "fieldValues"
    from (
        select
            nickname,
            "discordTag",
            "steamId",
            "riotId",
            "battleTag",
            "eaId",
            "ubisoftUsername",
            "psnId",
            "gamerTag",
            "friendCode",
            age,
            location,
            languages,
            microphone,
            weekday_from,
            weekday_to,
            weekend_from,
            weekend_to,
            timezone,
            platforms,
            platform,
            about,
            ambitions,
            "newOrReturning",
            updated,
            "updatedSeconds",
            total,
            field_key,
            field_ilk,
            field_label,
            field_icon,
            field_ordinal,
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'key', field_option_key,
                        'label', field_option_label
                    ) order by field_option_ordinal
                ) filter (where field_option_key is not null),
                '[]'
            ) as field_options
        from (
            select
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
                extract(year from age(player.birthday))::int as age,
                player.location,
                player.languages,
                player.microphone,
                player.weekday_from,
                player.weekday_to,
                player.weekend_from,
                player.weekend_to,
                player.timezone,
                game.platforms,
                profile.platform,
                profile.about,
                profile.ambitions,
                profile.new_or_returning as "newOrReturning",
                profile.updated::text,
                extract(epoch from (now() - updated))::int as "updatedSeconds",
                profile_filtered.total,
                field.ilk as field_ilk,
                field.key as field_key,
                field.label as field_label,
                field.icon as field_icon,
                field.ordinal as field_ordinal,
                field_option.key as field_option_key,
                field_option.label as field_option_label,
                field_option.ordinal as field_option_ordinal
            from player_profile as profile
                join game on game.id = profile.game_id
                join player on player.id = profile.player_id
                left join player_profile_field_value field_value on field_value.player_profile_id = profile.id
                left join player_profile_field_value_option field_value_option on field_value_option.player_profile_field_value_id = field_value.id
                left join field on field_value.field_id = field.id
                left join field_option on field_value_option.field_option_id = field_option.id
                join (
                    """ <> queryStringFilter handle page timezone filters fieldAndOptionIds <> """
                ) as profile_filtered on profile_filtered.id = profile.id
            order by profile.updated desc
        ) as profile
        group by
            nickname,
            "discordTag",
            "steamId",
            "riotId",
            "battleTag",
            "eaId",
            "ubisoftUsername",
            "psnId",
            "gamerTag",
            "friendCode",
            age,
            location,
            languages,
            microphone,
            weekday_from,
            weekday_to,
            weekend_from,
            weekend_to,
            timezone,
            platforms,
            platform,
            about,
            ambitions,
            "newOrReturning",
            updated,
            "updatedSeconds",
            total,
            field_key,
            field_ilk,
            field_label,
            field_icon,
            field_ordinal
    ) as profile
    group by
        nickname,
        "discordTag",
        "steamId",
        "riotId",
        "battleTag",
        "eaId",
        "ubisoftUsername",
        "psnId",
        "gamerTag",
        "friendCode",
        age,
        location,
        languages,
        microphone,
        weekday_from,
        weekday_to,
        weekend_from,
        weekend_to,
        timezone,
        platforms,
        platform,
        about,
        ambitions,
        "newOrReturning",
        updated,
        "updatedSeconds",
        total
) as profiles
group by total"""

loadProfiles
    :: ∀ errors
    .  Client
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Array FieldAndOptionIds
    -> Async (InternalTerror_ errors) (ViewPlayerProfilesByGame.OkContent)
loadProfiles client handle page timezone filters fieldAndOptionIds =
    queryFirstInternal_ client (queryString handle page timezone filters fieldAndOptionIds)
