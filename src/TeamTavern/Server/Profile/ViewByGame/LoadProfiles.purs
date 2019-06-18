module TeamTavern.Server.Profile.ViewByGame.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
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

type LoadProfilesDto =
    { nickname :: String
    , summary :: Array String
    , fieldValues :: Array
        { id :: Int
        , fieldId :: Int
        , data ::
            { url :: Maybe String
            , optionId :: Maybe Int
            , optionIds :: Maybe (Array Int)
            }
        }
    }

type LoadProfilesResult =
    { nickname :: Nickname
    , summary :: Summary
    , fieldValues :: Array
        { id :: Int
        , fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    }

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

queryString :: Query
queryString = Query """
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
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool handle = do
    result <- pool
        # query queryString (queryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ nickname, summary, fieldValues } ->
        { nickname: wrap nickname
        , summary: summary <#> wrap # wrap
        , fieldValues: fieldValues <#>
            \{ id, fieldId, data: { url, optionId, optionIds } } ->
                { id, fieldId, url, optionId, optionIds }
        }
