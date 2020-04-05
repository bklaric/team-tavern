module TeamTavern.Server.Profile.ViewGamePlayers.LoadProfiles
    (pageSize, LoadProfilesResult, LoadProfilesError, prepareString, queryStringWithoutPagination, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe(..))
import Data.MultiMap as MultiMap
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query_)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Routes (Age, Country, Filters, Handle, HasMicrophone, Language, ProfilePage, Time, Timezone)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as Key
import URI.Extra.QueryPairs as Value

pageSize :: Int
pageSize = 20

type LoadProfilesResult =
    { nickname :: String
    , age :: Maybe Int
    , country :: Maybe String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , fieldValues :: Array
        { field ::
            { key :: String
            , label :: String
            , icon :: String
            , ilk :: Int
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , summary :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

prepareString :: String -> String
prepareString stringValue =
       "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") stringValue)
    <> "'"

prepareJsonString :: String -> String
prepareJsonString stringValue =
       "\""
    <> (String.replace (String.Pattern "\"") (String.Replacement "") stringValue)
    <> "\""

prepareFilters :: QueryPairs Key Value -> Array (Tuple String (Array String))
prepareFilters (QueryPairs filters) = let
    prepareFilter fieldKey optionKey = let
        preparedFieldKey = prepareJsonString $ Key.keyToString fieldKey
        preparedOptionKey = prepareJsonString $ Value.valueToString optionKey
        in
        Tuple preparedFieldKey preparedOptionKey
    preparedFilters =
        filters # Array.mapMaybe \(Tuple fieldKey optionKey') ->
            optionKey' <#> \optionKey -> prepareFilter fieldKey optionKey
    groupedFilters = preparedFilters
        # foldl (\groupedFiltersSoFar (Tuple fieldKey optionKey) ->
            MultiMap.insert' fieldKey optionKey groupedFiltersSoFar)
            MultiMap.empty
    in
    groupedFilters # MultiMap.toUnfoldable'

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

createCountriesFilter :: Array Country -> String
createCountriesFilter [] = ""
createCountriesFilter countries = """ and player.country in (
    with recursive region_rec(name) as (
        select region.name from region where region.name = any(array[""" <> (countries <#> prepareString # intercalate ", ") <> """])
        union all
        select subregion.name from region as subregion join region_rec on subregion.superregion_name = region_rec.name
    )
    select * from region_rec)
"""

timezoneAdjustedTime :: Timezone -> String -> String
timezoneAdjustedTime timezone timeColumn =
    """((current_date || ' ' || """ <> timeColumn <> """ || ' ' || player.timezone)::timestamptz
    at time zone """ <> prepareString timezone <> """)::time"""

createWeekdayOnlineFilter :: Timezone -> Maybe Time -> Maybe Time -> String
createWeekdayOnlineFilter timezone (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time"
        playerStart = timezoneAdjustedTime timezone "player.weekday_from"
        playerEnd   = timezoneAdjustedTime timezone "player.weekday_to"
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
        playerStart = timezoneAdjustedTime timezone "player.weekend_from"
        playerEnd   = timezoneAdjustedTime timezone "player.weekend_to"
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
createMicrophoneFilter true = " and player.has_microphone"

createPlayerFilterString :: Timezone -> Filters -> String
createPlayerFilterString timezone filters =
    createAgeFilter filters.age.from filters.age.to
    <> createLanguagesFilter filters.languages
    <> createCountriesFilter filters.countries
    <> createWeekdayOnlineFilter
        timezone filters.weekdayOnline.from filters.weekdayOnline.to
    <> createWeekendOnlineFilter
        timezone filters.weekendOnline.from filters.weekendOnline.to
    <> createMicrophoneFilter filters.microphone

-- Example:
-- jsonb_path_exists(player_profile."fieldValues", '$[*] ? (@.field.key == "rank") ? (@.option.key == "guardian" || @.option.key == "herald" || @.options[*].key == "guardian" || @.options[*].key == "herald")')
-- and jsonb_path_exists(player_profile."fieldValues", '$[*] ? (@.field.label == "Region") ? (@.option.key == "eu-east" || @.options[*].key == "eu-east")')
createFieldsFilterString :: QueryPairs Key Value -> String
createFieldsFilterString fields = let
    fieldFilterString (Tuple fieldKey optionKeys) =
        optionKeys
        <#> (\optionKey -> "@.option.key == \"" <> optionKey <> "\" || @.options[*].key == \"" <> optionKey <> "\"")
        # intercalate " || "
        # \filterString -> "jsonb_path_exists(profile.\"fieldValues\", '$[*] ? (@.field.key == \"" <> fieldKey <> "\") ? (" <> filterString <> ")')"
    in
    fields
    # prepareFilters
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
            extract(year from age(player.birthday))::int as age,
            player.country,
            player.languages,
            player.has_microphone as "hasMicrophone",
            case
                when player.weekday_from is not null and player.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> timezoneAdjustedTime timezone "player.weekday_from" <> """, 'HH24:MI'),
                    'to', to_char(""" <> timezoneAdjustedTime timezone "player.weekday_to" <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when player.weekend_from is not null and player.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> timezoneAdjustedTime timezone "player.weekend_from" <> """, 'HH24:MI'),
                    'to', to_char(""" <> timezoneAdjustedTime timezone "player.weekend_to" <> """, 'HH24:MI')
                )
            end as "weekendOnline",
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'label', field_values.label,
                            'key', field_values.key,
                            'ilk', field_values.ilk,
                            'icon', field_values.icon
                        ),
                        case
                            when field_values.ilk = 1 then 'url'
                            when field_values.ilk = 2 then 'option'
                            when field_values.ilk = 2 then 'options'
                            when field_values.ilk = 3 then 'options'
                            when field_values.ilk = 3 then 'options'
                        end,
                        case
                            when field_values.ilk = 1 then field_values.url
                            when field_values.ilk = 2 then field_values.single
                            when field_values.ilk = 2 then field_values.multi
                            when field_values.ilk = 3 then field_values.multi
                            when field_values.ilk = 3 then field_values.multi
                        end
                    ) order by field_values.ordinal
                ) filter (where field_values.player_profile_id is not null),
                '[]'
            ) as "fieldValues",
            profile.summary,
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
                        jsonb_agg(jsonb_build_object(
                            'key', multi.key,
                            'label', multi.label
                        )) filter (where multi.label is not null),
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
                order by
                    field.ordinal
            ) as field_values
                on field_values.player_profile_id = profile.id
        where
            game.handle = """ <> prepareString handle
            <> createPlayerFilterString timezone filters <> """
        group by player.id, profile.id
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
