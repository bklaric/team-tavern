module TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles
    (pageSize, LoadProfilesResult, LoadProfilesError, queryStringWithoutPagination, loadProfiles) where

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
import TeamTavern.Server.Infrastructure.Postgres (prepareJsonString, prepareString, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)
import TeamTavern.Server.Profile.Routes (Age, Location, Filters, Handle, HasMicrophone, Language, ProfilePage, Time, Timezone, NewOrReturning)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as Key
import URI.Extra.QueryPairs as Value

pageSize :: Int
pageSize = 10

type LoadProfilesResult =
    { owner :: String
    , handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
    , weekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , weekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , about :: Array String
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
    , ambitions :: Array String
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

createAgeFilter :: Maybe Age -> Maybe Age -> String
createAgeFilter Nothing Nothing = ""
createAgeFilter (Just ageFrom) Nothing = " and profile.age_to >= " <> show ageFrom
createAgeFilter Nothing (Just ageTo) = " and profile.age_from <= " <> show ageTo
createAgeFilter (Just ageFrom) (Just ageTo) =
    " and (profile.age_to >= " <> show ageFrom <> " and profile.age_from <= " <> show ageTo <> ")"

createLanguagesFilter :: Array Language -> String
createLanguagesFilter [] = ""
createLanguagesFilter languages = " and profile.languages && (array[" <> (languages <#> prepareString # intercalate ", ") <> "])"

createCountriesFilter :: Array Location -> String
createCountriesFilter [] = ""
createCountriesFilter locations = """ and exists (
    (
        with recursive region_rec(name) as (
            select region.name from region where region.name = any(profile.locations)
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
        playerStart = teamAdjustedWeekdayFrom timezone
        playerEnd   = teamAdjustedWeekdayTo timezone
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
        playerStart = teamAdjustedWeekendFrom timezone
        playerEnd   = teamAdjustedWeekendTo timezone
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
createMicrophoneFilter true = " and profile.microphone"

createNewOrReturningFilter :: NewOrReturning -> String
createNewOrReturningFilter false = ""
createNewOrReturningFilter true = " and profile.new_or_returning"

createProfileFilterString :: Timezone -> Filters -> String
createProfileFilterString timezone filters =
    createAgeFilter filters.age.from filters.age.to
    <> createLanguagesFilter filters.languages
    <> createCountriesFilter filters.locations
    <> createWeekdayOnlineFilter
        timezone filters.weekdayOnline.from filters.weekdayOnline.to
    <> createWeekendOnlineFilter
        timezone filters.weekendOnline.from filters.weekendOnline.to
    <> createMicrophoneFilter filters.microphone
    <> createNewOrReturningFilter filters.newOrReturning

prepareFields :: QueryPairs Key Value -> Array (Tuple String (Array String))
prepareFields (QueryPairs filters) = let
    preparedField fieldKey optionKey = let
        preparedFieldKey = prepareJsonString $ Key.keyToString fieldKey
        preparedOptionKey = prepareJsonString $ Value.valueToString optionKey
        in
        Tuple preparedFieldKey preparedOptionKey
    preparedFields =
        filters # Array.mapMaybe \(Tuple fieldKey optionKey') ->
            optionKey' <#> \optionKey -> preparedField fieldKey optionKey
    groupeFields = preparedFields
        # foldl (\groupedFiltersSoFar (Tuple fieldKey optionKey) ->
            MultiMap.insertOrAppend' fieldKey optionKey groupedFiltersSoFar)
            MultiMap.empty
    in
    groupeFields # MultiMap.toUnfoldable'

-- Example:
-- jsonb_path_exists(team_profile."fieldValues", '$[*] ? (@.field.key == "rank") ? (@.options[*].key == "guardian" || @.options[*].key == "herald")')
-- and jsonb_path_exists(team_profile."fieldValues", '$[*] ? (@.field.label == "Region") ? (@.options[*].key == "eu-east")')
createFieldsFilterString :: QueryPairs Key Value -> String
createFieldsFilterString fields = let
    fieldFilterString (Tuple fieldKey optionKeys) =
        optionKeys
        <#> (\optionKey -> "@.options[*].key == " <> optionKey)
        # intercalate " || "
        # \filterString -> "jsonb_path_exists(profile.\"fieldValues\", '$[*] ? (@.field.key == " <> fieldKey <> ") ? (" <> filterString <> ")')"
    in
    fields
    # prepareFields
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
            team.name,
            team.website,
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
            team.about,
            profile.new_or_returning as "newOrReturning",
            profile.ambitions,
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
            player.email_confirmed
            and game.handle = """ <> prepareString handle
            <> createProfileFilterString timezone filters <> """
        group by player.id, team.id, profile.id
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
