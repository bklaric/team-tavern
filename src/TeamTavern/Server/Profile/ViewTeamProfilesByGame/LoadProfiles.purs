module TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles
    (pageSize, LoadProfilesResult, LoadProfilesError, createTeamFilterString, createFieldsFilterString, queryStringWithoutPagination, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (intercalate)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query_)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time, Field)
import TeamTavern.Routes.Shared.Organization (Organization, OrganizationNW)
import TeamTavern.Routes.Shared.Organization as Organization
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.Size as Size
import TeamTavern.Routes.Shared.Team (Contacts')
import TeamTavern.Routes.Shared.Timezone (Timezone)
import TeamTavern.Server.Infrastructure.Postgres (prepareJsonString, prepareString, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)
import TeamTavern.Server.Profile.Routes (Handle, ProfilePage)

pageSize :: Int
pageSize = 10

type LoadProfilesResult = Contacts'
    ( owner :: String
    , handle :: String
    , organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , weekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , size :: Size
    , allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , newOrReturning :: Boolean
    , about :: Array String
    , updated :: String
    , updatedSeconds :: Number
    )

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

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
        teamStart = teamAdjustedWeekdayFrom timezone
        teamEnd   = teamAdjustedWeekdayTo timezone
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
        teamStart = teamAdjustedWeekendFrom timezone
        teamEnd   = teamAdjustedWeekendTo timezone
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

-- Example:
-- jsonb_path_exists(team_profile."fieldValues", '$[*] ? (@.field.key == "rank") ? (@.options[*].key == "guardian" || @.options[*].key == "herald")')
-- and jsonb_path_exists(team_profile."fieldValues", '$[*] ? (@.field.label == "Region") ? (@.options[*].key == "eu-east")')
createFieldsFilterString :: Array Field -> String
createFieldsFilterString fields = let
    fieldFilterString ({ fieldKey, optionKeys }) =
        optionKeys
        <#> prepareJsonString
        <#> (\optionKey -> "@.options[*].key == " <> optionKey)
        # intercalate " || "
        # \filterString -> "jsonb_path_exists(profile.\"fieldValues\", '$[*] ? (@.field.key == " <> prepareJsonString fieldKey <> ") ? (" <> filterString <> ")')"
    in
    fields
    <#> fieldFilterString
    # intercalate " and "
    # \filterString ->
        if String.null filterString
        then ""
        else "where " <> filterString

queryStringWithoutPagination ::
    Handle -> Timezone -> Filters -> Query
queryStringWithoutPagination handle timezone filters = Query $ """
    select profile.*
    from
        (select
            player.nickname as owner,
            team.handle,
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
                        'name', team.name,
                        'website', team.website
                    )
                )
            end as organization,
            team.discord_tag as "discordTag",
            team.discord_server as "discordServer",
            team.steam_id as "steamId",
            team.riot_id as "riotId",
            team.battle_tag as "battleTag",
            team.psn_id as "psnId",
            team.gamer_tag as "gamerTag",
            team.friend_code as "friendCode",
            team.age_from as "ageFrom",
            team.age_to as "ageTo",
            team.locations,
            team.languages,
            team.microphone,
            case
                when team.weekday_from is not null and team.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> teamAdjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> teamAdjustedWeekdayTo timezone <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when team.weekend_from is not null and team.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> teamAdjustedWeekendFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> teamAdjustedWeekendTo timezone <> """, 'HH24:MI')
                )
            end as "weekendOnline",
            profile.size,
            json_build_object(
                'head', game.platforms[1],
                'tail', game.platforms[2:]
            ) as "allPlatforms",
            profile.platforms as "selectedPlatforms",
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'ilk', field_values.ilk,
                            'key', field_values.key,
                            'label', field_values.label,
                            'icon', field_values.icon
                        ),
                        'options', field_values.multi
                    ) order by field_values.ordinal
                ) filter (where field_values.team_profile_id is not null),
                '[]'
            ) as "fieldValues",
            profile.new_or_returning as "newOrReturning",
            profile.about,
            profile.updated::text,
            extract(epoch from (now() - profile.updated)) as "updatedSeconds"
        from team_profile as profile
            join game on game.id = profile.game_id
            join team on team.id = profile.team_id
            join player on player.id = team.owner_id
            left join (
                select field_value.team_profile_id,
                    field.ilk,
                    field.label,
                    field.key,
                    field.icon,
                    field.ordinal,
                    coalesce(
                        jsonb_agg(
                            jsonb_build_object(
                                'key', multi.key,
                                'label', multi.label
                            ) order by multi.ordinal
                        ) filter (where multi.label is not null),
                        '[]'
                    ) as multi
                from
                    field
                    join team_profile_field_value as field_value
                        on field_value.field_id = field.id
                    left join team_profile_field_value_option as field_value_option
                        on field_value_option.team_profile_field_value_id = field_value.id
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                where
                    field.ilk = 2 or field.ilk = 3
                group by
                    field.id,
                    field_value.id
            ) as field_values
                on field_values.team_profile_id = profile.id
        where
            game.handle = """ <> prepareString handle
            <> createTeamFilterString timezone filters <> """
        group by player.id, team.id, profile.id, game.id
        ) as profile
    """ <> createFieldsFilterString filters.fields <> """
    order by profile.updated desc"""

queryString :: Handle -> ProfilePage -> Timezone -> Filters -> Query
queryString handle page timezone filters =
    queryStringWithoutPagination handle timezone filters
    <> (Query $ """ limit """ <> show pageSize <> """ offset """ <> show ((page - 1) * pageSize))

loadProfiles
    :: forall errors
    .  Client
    -> Handle
    -> ProfilePage
    -> Timezone
    -> Filters
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles client handle page timezone filters = do
    result <- client
        # query_ (queryString handle page timezone filters)
        # label (SProxy :: SProxy "databaseError")
    profiles <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure profiles
