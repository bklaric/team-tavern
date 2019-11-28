module TeamTavern.Server.Profile.ViewByGame.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, sanitizeStringValue, createProfilesFilterString, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.MultiMap as MultiMap
import Data.Newtype (wrap)
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
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Routes (Handle, ProfileIlk)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as Key
import URI.Extra.QueryPairs as Value

type LoadProfilesDto =
    { nickname :: String
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

type LoadProfilesResult =
    { nickname :: Nickname
    , summary :: Summary
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

prepareFilters ::
    Array (Tuple Key (Maybe Value)) -> Array (Tuple String (Array String))
prepareFilters filters = let
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

createProfilesFilterString :: String -> ProfileIlk -> Array (Tuple Key (Maybe Value)) -> String
createProfilesFilterString preparedHandle ilk filters = let
    -- Prepare Array (Tuple String (Array String)) as filters.
    preparedFilters = prepareFilters filters
    in
    if Array.null preparedFilters
    then "where game.handle = " <> preparedHandle <> " and profile.type = " <> show ilk
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
                where game.handle = """ <> preparedHandle <> """ and profile.type = """ <> show ilk <> """
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

queryString :: Handle -> ProfileIlk -> QueryPairs Key Value -> Query
queryString handle ilk (QueryPairs filters) = let
    -- Prepare game handle.
    preparedHandle = sanitizeStringValue handle

    -- Create profiles filter string.
    filterString = createProfilesFilterString preparedHandle ilk filters

    -- Insert it into the rest of the query.
    in
    Query $ """
    select
        profile.id,
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
    order by profile.updated desc"""

loadProfiles
    :: forall errors
    .  Client
    -> Handle
    -> ProfileIlk
    -> QueryPairs Key Value
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles client handle ilk filters = do
    result <- client
        # query_ (queryString handle ilk filters)
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ nickname, summary, fieldValues, updated, updatedSeconds } ->
        { nickname: wrap nickname
        , summary: summary <#> wrap # wrap
        , fieldValues
        , updated
        , updatedSeconds
        }
