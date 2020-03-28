module TeamTavern.Server.Profile.ViewByGame.LoadProfiles
    (pageSize, pageSize', LoadProfilesResult, LoadProfilesError, sanitizeStringValue, createProfilesFilterString, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MultiMap as MultiMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query_)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Routes (Age, Filters, Handle, HasMicrophone, Language, ProfileIlk, ProfilePage, Time, Country)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as Key
import URI.Extra.QueryPairs as Value

pageSize :: Int
pageSize = 20

pageSize' :: Number
pageSize' = toNumber pageSize

type LoadProfilesResult =
    { nickname :: String
    , age :: Maybe Int
    , languages :: Array String
    , hasMicrophone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , summary :: Array String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
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

sanitizeTableName :: String -> String
sanitizeTableName tableName = "\""
    <> (String.replace (String.Pattern "\"") (String.Replacement "") tableName)
    <> "\""

sanitizeStringValue :: String -> String
sanitizeStringValue stringValue = "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") stringValue)
    <> "'"

prepareFilters :: QueryPairs Key Value -> Array (Tuple String (Array String))
prepareFilters (QueryPairs filters) = let
    prepareFilter fieldKey optionKey = let
        preparedFieldKey = sanitizeTableName $ Key.keyToString fieldKey
        preparedOptionKey = sanitizeStringValue $ Value.valueToString optionKey
        in
        Tuple preparedFieldKey preparedOptionKey
    sanitizedFilters =
        filters # Array.mapMaybe \(Tuple fieldKey optionKey') ->
            optionKey' <#> \optionKey -> prepareFilter fieldKey optionKey
    groupedFilters = sanitizedFilters
        # foldl (\groupedFiltersSoFar (Tuple fieldKey optionKey) ->
            MultiMap.insert' fieldKey optionKey groupedFiltersSoFar)
            MultiMap.empty
    in
    groupedFilters # MultiMap.toUnfoldable' # Array.sortWith Tuple.fst

createFilterString :: Array (Tuple String (Array String)) -> String
createFilterString filters = let
    fieldFilterString (Tuple fieldKey optionKeys) =
        optionKeys
        <#> (\optionKey -> optionKey <> " = any(" <> fieldKey <> ")")
        # intercalate " or "
        # \fieldFilter -> "(" <> fieldFilter <> ")"
    in
    filters
    <#> fieldFilterString
    # intercalate " and "
    # \filterString ->
        if String.null filterString
        then ""
        else "where " <> filterString

createCrosstabColumns :: Array (Tuple String (Array String)) -> String
createCrosstabColumns filters =
    filters
    <#> (\(Tuple fieldKey _) -> fieldKey <> " text[]")
    # intercalate ", "
    # \crosstabColumns ->
        if String.null crosstabColumns
        then ""
        else ", " <> crosstabColumns

createCrosstabFieldsFilter :: Array (Tuple String (Array String)) -> String
createCrosstabFieldsFilter filters =
    filters
    <#> (Tuple.fst >>> String.replaceAll (Pattern "\"") (Replacement "'"))
    # intercalate ", "
    # \crosstabFieldsFilter -> "(" <> crosstabFieldsFilter <> ")"

createAgeFilter :: Maybe Age -> Maybe Age -> String
createAgeFilter Nothing Nothing = ""
createAgeFilter (Just ageFrom) Nothing = " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years')"
createAgeFilter Nothing (Just ageTo) = " and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"
createAgeFilter (Just ageFrom) (Just ageTo) =
    " and player.birthday < (current_timestamp - interval '" <> show ageFrom <> " years') "
    <> "and player.birthday > (current_timestamp - interval '" <> show (ageTo + 1) <> " years')"

createLanguagesFilter :: Array Language -> String
createLanguagesFilter [] = ""
createLanguagesFilter languages = " and player.languages && (array[" <> (languages <#> sanitizeStringValue # intercalate ", ") <> "])"

createCountriesFilter :: Array Country -> String
createCountriesFilter [] = ""
createCountriesFilter countries = """ and player.country in (
    with recursive region_rec(name) as (
        select region.name from region where region.name = any(array[""" <> (countries <#> sanitizeStringValue # intercalate ", ") <> """])
        union all
        select subregion.name from region as subregion join region_rec on subregion.superregion_name = region_rec.name
    )
    select * from region_rec)
"""

createWeekdayOnlineFilter :: Maybe Time -> Maybe Time -> String
createWeekdayOnlineFilter (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time" in
    """ and
    case
        when player.weekday_start < player.weekday_end and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekday_end and player.weekday_start < """ <> toTime <> """
        when player.weekday_start > player.weekday_end and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekday_end or player.weekday_start < """ <> toTime <> """
        when player.weekday_start < player.weekday_end and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekday_end or player.weekday_start < """ <> toTime <> """
        when player.weekday_start > player.weekday_end and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekdayOnlineFilter _ _ = ""

createWeekendOnlineFilter :: Maybe Time -> Maybe Time -> String
createWeekendOnlineFilter (Just from) (Just to) =
    let fromTime = "'" <> from <> "'::time"
        toTime   = "'" <> to   <> "'::time" in
    """ and
    case
        when player.weekend_start < player.weekend_end and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekend_end and player.weekend_start < """ <> toTime <> """
        when player.weekend_start > player.weekend_end and """ <> fromTime <> """ < """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekend_end or player.weekend_start < """ <> toTime <> """
        when player.weekend_start < player.weekend_end and """ <> fromTime <> """ > """ <> toTime <> """ then
            """ <> fromTime <> """ < player.weekend_end or player.weekend_start < """ <> toTime <> """
        when player.weekend_start > player.weekend_end and """ <> fromTime <> """ > """ <> toTime <> """ then
            true
    end
    """
createWeekendOnlineFilter _ _ = ""

createMicrophoneFilter :: HasMicrophone -> String
createMicrophoneFilter false = ""
createMicrophoneFilter true = " and player.has_microphone"

createProfilesFilterString :: Handle -> ProfileIlk -> Filters -> String
createProfilesFilterString handle ilk filters = let
    -- Prepare game handle.
    preparedHandle = sanitizeStringValue handle

    -- Prepare Array (Tuple String (Array String)) as filters.
    preparedFilters = prepareFilters filters.fields
    in
    if Array.null preparedFilters
    then "where game.handle = " <> preparedHandle
        <> " and profile.type = " <> show ilk
        <> createAgeFilter filters.age.from filters.age.to
        <> createLanguagesFilter filters.languages
        <> createCountriesFilter filters.countries
        <> createWeekdayOnlineFilter filters.weekdayOnline.from filters.weekdayOnline.to
        <> createWeekendOnlineFilter filters.weekendOnline.from filters.weekendOnline.to
        <> createMicrophoneFilter filters.microphone
    else let
        -- Create filter string.
        filterString = createFilterString preparedFilters

        -- Create crosstab column names.
        crosstabColumns = createCrosstabColumns preparedFilters

        -- Create crosstab fields filter.
        crosstabFieldsFilter = createCrosstabFieldsFilter preparedFilters
        in
        """
        where profile.id in (
            select id
            from crosstab(
                $$
                select
                    profile.id,
                    field.key as field,
                    array_agg(field_option.key) as options
                from profile
                join game on game.id = profile.game_id
                join field_value on field_value.profile_id = profile.id
                left join field_value_option on field_value_option.field_value_id = field_value.id
                join field on field.id = field_value.field_id
                join field_option on field_option.id = field_value.field_option_id
                    or field_option.id = field_value_option.field_option_id
                where game.handle = """ <> preparedHandle
                    <> """ and profile.type = """ <> show ilk
                    <> createAgeFilter filters.age.from filters.age.to
                    <> createLanguagesFilter filters.languages
                    <> createCountriesFilter filters.countries
                    <> createWeekdayOnlineFilter filters.weekdayOnline.from filters.weekdayOnline.to
                    <> createWeekendOnlineFilter filters.weekendOnline.from filters.weekendOnline.to
                    <> createMicrophoneFilter filters.microphone <> """
                group by profile.id, field.key
                order by profile.created;
                $$,
                $$
                select field.key
                from field
                join game on game.id = field.game_id
                where (field.type = 2 or field.type = 3)
                    and game.handle = """ <> preparedHandle <> """
                    and field.key in """ <> crosstabFieldsFilter <> """
                order by field.key;
                $$
            ) as (id int """ <> crosstabColumns <> """)
            """ <> filterString <> """
        )"""

queryString :: Handle -> ProfileIlk -> ProfilePage -> Filters -> Query
queryString handle ilk page filters = let
    -- Create profiles filter string.
    filterString = createProfilesFilterString handle ilk filters

    -- Insert it into the rest of the query.
    in
    Query $ """
    select
        profile.id,
        extract(year from age(player.birthday))::int as age,
        player.languages,
        player.has_microphone as "hasMicrophone",
        case
            when player.weekday_start is not null
            then json_build_object(
                'from', to_char(player.weekday_start, 'HH24:MI'),
                'to', to_char(player.weekday_end, 'HH24:MI')
            )
        end as "weekdayOnline",
        case
            when player.weekend_start is not null
            then json_build_object(
                'from', to_char(player.weekend_start, 'HH24:MI'),
                'to', to_char(player.weekend_end, 'HH24:MI')
            )
        end as "weekendOnline",
        game.handle,
        player.nickname,
        profile.summary,
        coalesce(field_values.field_values, '[]') as "fieldValues",
        profile.updated::text,
        extract(epoch from (now() - updated)) as "updatedSeconds"
    from profile
        join game on game.id = profile.game_id
        join player on player.id = profile.player_id
        left join field_values on field_values.profile_id = profile.id
    """ <> filterString <> """
    order by profile.updated desc
    limit """ <> show pageSize <> """ offset """ <> show ((page - 1) * pageSize)

loadProfiles
    :: forall errors
    .  Client
    -> Handle
    -> ProfileIlk
    -> ProfilePage
    -> Filters
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles client handle ilk page filters = do
    result <- client
        # query_ (queryString handle ilk page filters)
        # label (SProxy :: SProxy "databaseError")
    profiles <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles
