module TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles
    (createTeamFilterString, createFieldsFilterString, createFieldsJoinString, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (fold, intercalate, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Postgres.Client (Client)
import Postgres.Query (Query(..))
import TeamTavern.Routes.Profile.Shared (ProfilePage, pageSize)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame as ViewTeamProfilesByGame
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Organization as Organization
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.Size as Size
import TeamTavern.Routes.Shared.Types (Timezone, Handle)
import TeamTavern.Server.Infrastructure.Postgres (adjustedWeekdayFrom, adjustedWeekdayTo, adjustedWeekendFrom, adjustedWeekendTo, prepareString, queryFirstInternal_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Profile.Infrastructure.LoadFieldAndOptionIds (FieldAndOptionIds)

createOrganizationsFilter :: Array Organization -> String
createOrganizationsFilter [] = ""
createOrganizationsFilter organizations = " and team.organization = any (array[" <> (organizations <#> Organization.toString <#> prepareString # intercalate ", ") <> "])"

createAgeFilter :: Maybe Age -> Maybe Age -> String
createAgeFilter Nothing Nothing = ""
createAgeFilter (Just ageFrom) Nothing = """ and
    case
        when team.age_from is not null and team.age_to is not null
        then """ <> show ageFrom <> """ <= team.age_to
        when team.age_from is not null and team.age_to is null
        then true
        when team.age_from is null and team.age_to is not null
        then """ <> show ageFrom <> """ <= team.age_to
        when team.age_from is null and team.age_to is null
        then false
    end"""
createAgeFilter Nothing (Just ageTo) = """ and
    case
        when team.age_from is not null and team.age_to is not null
        then team.age_from <= """ <> show ageTo <> """
        when team.age_from is not null and team.age_to is null
        then team.age_from <= """ <> show ageTo <> """
        when team.age_from is null and team.age_to is not null
        then true
        when team.age_from is null and team.age_to is null
        then false
    end"""
createAgeFilter (Just ageFrom) (Just ageTo) = """ and
    case
        when team.age_from is not null and team.age_to is not null
        then """ <> show ageFrom <> """ <= team.age_to and team.age_from <= """ <> show ageTo <> """
        when team.age_from is not null and team.age_to is null
        then team.age_from <= """ <> show ageTo <> """
        when team.age_from is null and team.age_to is not null
        then """ <> show ageFrom <> """ <= team.age_to
        when team.age_from is null and team.age_to is null
        then false
    end"""

createLanguagesFilter :: Array Language -> String
createLanguagesFilter [] = ""
createLanguagesFilter languages = " and team.languages && (array[" <> (languages <#> prepareString # intercalate ", ") <> "])"

createCountriesFilter :: Array Location -> String
createCountriesFilter [] = ""
createCountriesFilter locations = """ and exists (
    (
        with recursive region_rec(name) as (
            select region.name from region where region.name = any(team.locations)
            union all
            select subregion.name from region as subregion join region_rec on subregion.superregion_name = region_rec.name
        )
        select * from region_rec
    )
    intersect (
        with recursive region_rec(name) as (
            select region.name from region where region.name = any(array[""" <> (locations <#> prepareString # intercalate ", ") <> """])
            union all
            select subregion.name from region as subregion join region_rec on subregion.superregion_name = region_rec.name
        )
        select * from region_rec
    )
)
"""

createWeekdayOnlineFilter :: Timezone -> Maybe Time -> Maybe Time -> String
createWeekdayOnlineFilter timezone (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time"
        teamStart = adjustedWeekdayFrom timezone
        teamEnd   = adjustedWeekdayTo timezone
    in
    """ and
    case
        when """ <> teamStart <> """ < """ <> teamEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ and """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ > """ <> teamEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ or """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ < """ <> teamEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ or """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ > """ <> teamEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekdayOnlineFilter _ _ _ = ""

createWeekendOnlineFilter :: Timezone -> Maybe Time -> Maybe Time -> String
createWeekendOnlineFilter timezone (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time"
        teamStart = adjustedWeekendFrom timezone
        teamEnd   = adjustedWeekendTo timezone
    in
    """ and
    case
        when """ <> teamStart <> """ < """ <> teamEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ and """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ > """ <> teamEnd <> """ and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ or """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ < """ <> teamEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < """ <> teamEnd <> """ or """ <> teamStart <> """ < """ <> toTime <> """
        when """ <> teamStart <> """ > """ <> teamEnd <> """ and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekendOnlineFilter _ _ _ = ""

createMicrophoneFilter :: HasMicrophone -> String
createMicrophoneFilter false = ""
createMicrophoneFilter true = " and team.microphone"

createSizesFilter :: Array Size -> String
createSizesFilter [] = ""
createSizesFilter sizes = " and profile.size = any (array[" <> (sizes <#> Size.toString <#> prepareString # intercalate ", ") <> "])"

createPlatformsFilter :: Array Platform -> String
createPlatformsFilter [] = ""
createPlatformsFilter platforms = " and profile.platforms && (array[" <> (platforms <#> Platform.toString <#> prepareString # intercalate ", ") <> "])"

createNewOrReturningFilter :: NewOrReturning -> String
createNewOrReturningFilter false = ""
createNewOrReturningFilter true = " and profile.new_or_returning"

createTeamFilterString :: Timezone -> Filters -> String
createTeamFilterString timezone filters =
    createOrganizationsFilter filters.organizations
    <> createAgeFilter filters.ageFrom filters.ageTo
    <> createLanguagesFilter filters.languages
    <> createCountriesFilter filters.locations
    <> createWeekdayOnlineFilter timezone filters.weekdayFrom filters.weekdayTo
    <> createWeekendOnlineFilter timezone filters.weekendFrom filters.weekendTo
    <> createMicrophoneFilter filters.microphone
    <> createSizesFilter filters.sizes
    <> createPlatformsFilter filters.platforms
    <> createNewOrReturningFilter filters.newOrReturning

createFieldsJoinString :: Array FieldAndOptionIds -> String
createFieldsJoinString fields = fields # mapWithIndex (\index _ -> let
    fieldValue = "field_value_" <> show index
    fieldValueOption = "field_value_option_" <> show index
    in """
    left join team_profile_field_value """ <> fieldValue <> """ on """ <> fieldValue <> """.team_profile_id = profile.id
    left join team_profile_field_value_option """ <> fieldValueOption <> """ on """ <> fieldValueOption <> """.team_profile_field_value_id = """ <> fieldValue <> """.id
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
    from team_profile profile
    join team on team.id = profile.team_id
    join game on game.id = profile.game_id """
    <> createFieldsJoinString fieldAndOptionIds <> """
    where game.handle = """ <> prepareString handle
    <> createTeamFilterString timezone filters
    <> createFieldsFilterString fieldAndOptionIds
    <> """
    group by profile.id, profile.updated
    order by profile.updated desc
    limit """ <> show pageSize <> """ offset """
    <> show ((max 0 (page - 1)) * pageSize)

queryString :: Handle -> ProfilePage -> Timezone -> Filters -> Array FieldAndOptionIds -> Query
queryString handle page timezone filters fieldAndOptionIds = Query $ """
select total::int as count, json_agg(json_build_object(
    'owner', "owner",
    'handle', "handle",
    'organization', "organization",
    'discordTag', "discordTag",
    'discordServer', "discordServer",
    'steamId', "steamId",
    'riotId', "riotId",
    'battleTag', "battleTag",
    'eaId', "eaId",
    'ubisoftUsername', "ubisoftUsername",
    'psnId', "psnId",
    'gamerTag', "gamerTag",
    'friendCode', "friendCode",
    'ageFrom', "ageFrom",
    'ageTo', "ageTo",
    'locations', "locations",
    'languages', "languages",
    'microphone', "microphone",
    'weekdayOnline', "weekdayOnline",
    'weekendOnline', "weekendOnline",
    'selectedPlatforms', "selectedPlatforms",
    'allPlatforms', "allPlatforms",
    'size', "size",
    'about', "about",
    'ambitions', "ambitions",
    'newOrReturning', "newOrReturning",
    'updated', "updated",
    'updatedSeconds', "updatedSeconds",
    'fieldValues', "fieldValues"
) order by updated desc) as profiles
from (
    select
        "owner",
        "handle",
        case
            when organization = 'informal'
            then json_build_object(
                'type', '"informal"'::jsonb,
                'value', '{}'::jsonb
            )
            when organization = 'organized'
            then json_build_object(
                'type', '"organized"'::jsonb,
                'value', json_build_object(
                    'name', name,
                    'website', website
                )
            )
        end as "organization",
        "discordTag",
        "discordServer",
        "steamId",
        "riotId",
        "battleTag",
        "eaId",
        "ubisoftUsername",
        "psnId",
        "gamerTag",
        "friendCode",
        "ageFrom",
        "ageTo",
        "locations",
        "languages",
        "microphone",
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
            'head', "allPlatforms"[1],
            'tail', "allPlatforms"[2:]
        ) as "allPlatforms",
        "selectedPlatforms",
        "size",
        "about",
        "ambitions",
        "newOrReturning",
        "updated",
        "updatedSeconds",
        "total",
        coalesce(
            jsonb_agg(
                jsonb_build_object(
                    'field', jsonb_build_object(
                        'ilk', field_ilk,
                        'key', field_key,
                        'label', field_label,
                        'icon', field_icon
                    ),
                    'options', field_options
                ) order by field_ordinal
            ) filter (where field_key is not null),
            '[]'
        ) as "fieldValues"
    from (
        select
            "owner",
            "handle",
            "organization",
            "name",
            "website",
            "discordTag",
            "discordServer",
            "steamId",
            "riotId",
            "battleTag",
            "eaId",
            "ubisoftUsername",
            "psnId",
            "gamerTag",
            "friendCode",
            "ageFrom",
            "ageTo",
            "locations",
            "languages",
            "microphone",
            "weekday_from",
            "weekday_to",
            "weekend_from",
            "weekend_to",
            "timezone",
            "allPlatforms",
            "selectedPlatforms",
            "size",
            "about",
            "ambitions",
            "newOrReturning",
            "updated",
            "updatedSeconds",
            "total",
            "field_key",
            "field_ilk",
            "field_label",
            "field_icon",
            "field_ordinal",
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
                player.nickname as owner,
                team.handle,
                team.organization,
                team."name",
                team."website",
                team.discord_tag as "discordTag",
                team.discord_server as "discordServer",
                team.steam_id as "steamId",
                team.riot_id as "riotId",
                team.battle_tag as "battleTag",
                team.ea_id as "eaId",
                team.ubisoft_username as "ubisoftUsername",
                team.psn_id as "psnId",
                team.gamer_tag as "gamerTag",
                team.friend_code as "friendCode",
                team.age_from as "ageFrom",
                team.age_to as "ageTo",
                team.locations,
                team.languages,
                team.microphone,
                team.weekday_from,
                team.weekday_to,
                team.weekend_from,
                team.weekend_to,
                team.timezone,
                profile.size,
                game.platforms as "allPlatforms",
                profile.platforms as "selectedPlatforms",
                profile.new_or_returning as "newOrReturning",
                profile.about,
                profile.ambitions,
                profile.updated::text,
                extract(epoch from (now() - profile.updated))::int as "updatedSeconds",
                profile_filtered.total,
                field.ilk as field_ilk,
                field.key as field_key,
                field.label as field_label,
                field.icon as field_icon,
                field.ordinal as field_ordinal,
                field_option.key as field_option_key,
                field_option.label as field_option_label,
                field_option.ordinal as field_option_ordinal
            from team_profile as profile
                join game on game.id = profile.game_id
                join team on team.id = profile.team_id
                join player on player.id = team.owner_id
                left join team_profile_field_value field_value on field_value.team_profile_id = profile.id
                left join team_profile_field_value_option field_value_option on field_value_option.team_profile_field_value_id = field_value.id
                left join field on field_value.field_id = field.id
                left join field_option on field_value_option.field_option_id = field_option.id
                join (
                    """ <> queryStringFilter handle page timezone filters fieldAndOptionIds <> """
                ) as profile_filtered on profile_filtered.id = profile.id
            order by profile.updated desc
        ) as profile
        group by
            "owner",
            "handle",
            "organization",
            "name",
            "website",
            "discordTag",
            "discordServer",
            "steamId",
            "riotId",
            "battleTag",
            "eaId",
            "ubisoftUsername",
            "psnId",
            "gamerTag",
            "friendCode",
            "ageFrom",
            "ageTo",
            "locations",
            "languages",
            "microphone",
            "weekday_from",
            "weekday_to",
            "weekend_from",
            "weekend_to",
            "timezone",
            "allPlatforms",
            "selectedPlatforms",
            "size",
            "about",
            "ambitions",
            "newOrReturning",
            "updated",
            "updatedSeconds",
            "total",
            "field_key",
            "field_ilk",
            "field_label",
            "field_icon",
            "field_ordinal"
    ) as profile
    group by
        "owner",
        "handle",
        "organization",
        "name",
        "website",
        "discordTag",
        "discordServer",
        "steamId",
        "riotId",
        "battleTag",
        "eaId",
        "ubisoftUsername",
        "psnId",
        "gamerTag",
        "friendCode",
        "ageFrom",
        "ageTo",
        "locations",
        "languages",
        "microphone",
        "weekday_from",
        "weekday_to",
        "weekend_from",
        "weekend_to",
        "timezone",
        "allPlatforms",
        "selectedPlatforms",
        "size",
        "about",
        "ambitions",
        "newOrReturning",
        "updated",
        "updatedSeconds",
        "total"
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
    -> Async (InternalTerror_ errors) (ViewTeamProfilesByGame.OkContent)
loadProfiles client handle page timezone filters fieldAndOptionIds =
    queryFirstInternal_ client (queryString handle page timezone filters fieldAndOptionIds)
