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
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Routes (Nickname)

type LoadProfilesDto =
    { handle :: String
    , title :: String
    , summary :: Array String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , fields :: Array
        { key :: String
        , type :: Int
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
    , updated :: String
    , updatedSeconds :: Number
    }

type LoadProfilesResult =
    { handle :: Handle
    , title :: Title
    , summary :: Summary
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , fields :: Array
        { key :: String
        , type :: Int
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
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

queryString :: Query
queryString = Query """
    select
        game.handle,
        game.title,
        profile.summary,
        coalesce(fields.fields, '[]') as "fields",
        coalesce(field_values.field_values, '[]') as "fieldValues",
        profile.updated::text,
        extract(epoch from (now() - updated)) as "updatedSeconds"
    from profile
        join player on player.id = profile.player_id
        join game on game.id = profile.game_id
        left join fields on fields.game_id = game.id
        left join field_values on field_values.profile_id = profile.id
    where lower(player.nickname) = lower($1)
    order by profile.updated desc;
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
    pure $ profiles <#> \{ handle, title, summary, fieldValues, fields, updated, updatedSeconds } ->
        { handle: wrap handle
        , title: wrap title
        , summary: summary <#> wrap # wrap
        , fieldValues
        , fields
        , updated
        , updatedSeconds
        }
