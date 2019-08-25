module TeamTavern.Server.Profile.ViewByGame.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Domain.Summary (Summary)
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
    }

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

queryString :: QueryPairs Key Value -> Query
queryString (QueryPairs filters) = let
    filters' = filters # mapMaybe \(Tuple key value') ->
        case value' of
        Nothing -> Nothing
        Just value -> Just $ Tuple (Key.keyToString key) (Value.valueToString value)
    -- filterString =
    --     if Array.null filters'
    --     then ""
    --     else filters' # intercalateMap " and " \(Tuple fieldKey optionKey) ->
    --         "(field.key = " <>
    in


    Query """
    select
        game.handle,
        player.nickname,
        profile.summary,
        coalesce(
            json_agg(json_build_object(
                'id', field_value.id,
                'fieldId', field_value.field_id,
                'data', field_value.data
            ))
            filter (where field_value.id is not null),
            '[]'
        ) as "fieldValues"
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    left join field_value on field_value.profile_id = profile.id
    left join field on field.id = field_value.field_id
    where game.handle = $1
    group by game.handle, player.nickname, profile.summary, profile.created
    order by profile.created desc
    """

queryParameters :: Handle -> Array QueryParameter
queryParameters handle = handle : []

loadProfiles
    :: forall errors
    .  Pool
    -> Handle
    -> QueryPairs Key Value
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool handle filters = do
    result <- pool
        # query (queryString filters) (queryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ nickname, summary, fieldValues } ->
        { nickname: wrap nickname
        , summary: summary <#> wrap # wrap
        , fieldValues
        }
