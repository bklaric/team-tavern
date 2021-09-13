module TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles
    (pageSize, LoadProfilesResult, LoadProfilesError, createPlayerFilterString, createFieldsFilterString, queryStringWithoutPagination, loadProfiles) where

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
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.PlayerBase (PlayerBaseRow)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsRow)
import TeamTavern.Routes.Shared.PlayerDetails (PlayerDetailsRow)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileRow)
import TeamTavern.Routes.Shared.Timezone (Timezone)
import TeamTavern.Server.Infrastructure.Postgres (playerAdjustedWeekdayFrom, playerAdjustedWeekdayTo, playerAdjustedWeekendFrom, playerAdjustedWeekendTo, prepareJsonString, prepareString)
import TeamTavern.Server.Profile.Routes (Handle, ProfilePage)
import Type.Row (type (+))

pageSize :: Int
pageSize = 10

type LoadProfilesResult = Record (PlayerBaseRow + PlayerContactsRow + PlayerDetailsRow + PlayerProfileRow + ())

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

createAgeFilter :: Maybe Age -> Maybe Age -> String
createAgeFilter Nothing Nothing = ""
createAgeFilter (Just ageFrom) Nothing = " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years')"
createAgeFilter Nothing (Just ageTo) = " and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"
createAgeFilter (Just ageFrom) (Just ageTo) =
    " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years') "
    <> "and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"

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
        playerStart = playerAdjustedWeekdayFrom timezone
        playerEnd   = playerAdjustedWeekdayTo timezone
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
        playerStart = playerAdjustedWeekendFrom timezone
        playerEnd   = playerAdjustedWeekendTo timezone
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

-- Example:
-- jsonb_path_exists(player_profile."fieldValues", '$[*] ? (@.field.key == "rank") ? (@.option.key == "guardian" || @.option.key == "herald" || @.options[*].key == "guardian" || @.options[*].key == "herald")')
-- and jsonb_path_exists(player_profile."fieldValues", '$[*] ? (@.field.label == "Region") ? (@.option.key == "eu-east" || @.options[*].key == "eu-east")')
createFieldsFilterString :: Array Field -> String
createFieldsFilterString fields = let
    fieldFilterString ({ fieldKey, optionKeys }) =
        optionKeys
        <#> prepareJsonString
        <#> (\optionKey -> "@.option.key == " <> optionKey <> " || @.options[*].key == " <> optionKey)
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
            player.nickname,
            player.discord_tag as "discordTag",
            player.steam_id as "steamId",
            player.riot_id as "riotId",
            player.battle_tag as "battleTag",
            player.psn_id as "psnId",
            player.gamer_tag as "gamerTag",
            player.friend_code as "friendCode",
            extract(year from age(player.birthday))::int as age,
            player.location,
            player.languages,
            player.microphone,
            case
                when player.weekday_from is not null and player.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> playerAdjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> playerAdjustedWeekdayTo timezone <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when player.weekend_from is not null and player.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> playerAdjustedWeekendFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> playerAdjustedWeekendTo timezone <> """, 'HH24:MI')
                )
            end as "weekendOnline",
            json_build_object(
                'head', game.platforms[1],
                'tail', game.platforms[2:]
            ) as platforms,
            profile.platform as "platform",
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'ilk', field_values.ilk,
                            'key', field_values.key,
                            'label', field_values.label,
                            'icon', field_values.icon
                        ),
                        case
                            when field_values.ilk = 1 then 'url'
                            when field_values.ilk = 2 then 'option'
                            when field_values.ilk = 3 then 'options'
                        end,
                        case
                            when field_values.ilk = 1 then field_values.url
                            when field_values.ilk = 2 then field_values.single
                            when field_values.ilk = 3 then field_values.multi
                        end
                    ) order by field_values.ordinal
                ) filter (where field_values.player_profile_id is not null),
                '[]'
            ) as "fieldValues",
            profile.about,
            profile.ambitions,
            profile.new_or_returning as "newOrReturning",
            profile.updated::text,
            extract(epoch from (now() - updated)) as "updatedSeconds"
        from player_profile as profile
            join game on game.id = profile.game_id
            join player on player.id = profile.player_id
            left join (
                select field_value.player_profile_id,
                    field.ilk,
                    field.label,
                    field.key,
                    field.icon,
                    field.ordinal,
                    to_jsonb(field_value.url) as url,
                    jsonb_build_object(
                        'key', single.key,
                        'label', single.label
                    ) as single,
                    coalesce(
                        jsonb_agg(
                            jsonb_build_object(
                                'key', multi.key,
                                'label', multi.label
                            ) order by multi.ordinal
                        ) filter (where multi.label is not null),
                        '[]'
                    ) as multi
                from field
                    join player_profile_field_value as field_value on field_value.field_id = field.id
                    left join player_profile_field_value_option as field_value_option
                        on field_value_option.player_profile_field_value_id = field_value.id
                    left join field_option as single
                        on single.id = field_value.field_option_id
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                group by
                    field.id,
                    field_value.id,
                    single.id
            ) as field_values
                on field_values.player_profile_id = profile.id
        where
            game.handle = """ <> prepareString handle
            <> createPlayerFilterString timezone filters <> """
        group by player.id, game.id, profile.id
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
