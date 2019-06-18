module TeamTavern.Server.Profile.ViewByPlayer.LoadProfiles
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
import TeamTavern.Server.Game.Domain.Title (Title)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Domain.Summary (Summary)

type LoadProfilesDto =
    { handle :: String
    , title :: String
    , summary :: Array String
    , fieldValues :: Array
        { fieldId :: Int
        , data ::
            { url :: Maybe String
            , optionId :: Maybe Int
            , optionIds :: Maybe (Array Int)
            }
        }
    , fields :: Array
        { id :: Int
        , type :: Int
        , label :: String
        , data ::
            { options :: Maybe (Array
                { id :: Int
                , option :: String
                })
            }
        }
    }

type LoadProfilesResult =
    { handle :: Handle
    , title :: Title
    , summary :: Summary
    , fieldValues :: Array
        { fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    , fields :: Array
        { id :: Int
        , type :: Int
        , label :: String
        , options :: Maybe (Array
            { id :: Int
            , option :: String
            })
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
        game.title,
        profile.summary,
        coalesce(
            json_agg(json_build_object(
                'fieldId', field_value.field_id,
                'data', field_value.data
            ))
            filter (where field_value.id is not null),
            '[]'
        ) as "fieldValues",
        coalesce(
            json_agg(field order by field.id)
            filter (where field.id is not null),
            '[]'
        ) as "fields"
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    left join field on field.game_id = game.id
    left join field_value on field_value.profile_id = profile.id
        and field_value.field_id = field.id
    where player.nickname = $1
    group by game.handle, game.title, profile.summary, profile.created
    order by profile.created desc
    """

queryParameters :: Nickname -> Array QueryParameter
queryParameters nickname = nickname : []

loadProfiles
    :: forall errors
    .  Pool
    -> Nickname
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool nickname = do
    result <- pool
        # query queryString (queryParameters nickname)
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ handle, title, summary, fieldValues, fields } ->
        { handle: wrap handle
        , title: wrap title
        , summary: summary <#> wrap # wrap
        , fieldValues: fieldValues <#>
            \{ fieldId, data: { url, optionId, optionIds } } ->
                { fieldId, url, optionId, optionIds }
        , fields: fields <#> \{ id, type, label, data: { options } } ->
            { id, type, label, options }
        }
