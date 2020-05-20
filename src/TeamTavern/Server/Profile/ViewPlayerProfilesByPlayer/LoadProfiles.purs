module TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Routes (Nickname)

type LoadProfilesResult =
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
        , ilk :: Int
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , updated :: String
    , updatedSeconds :: Number
    }

type LoadProfilesError errors
    = Variant
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
    from player_profile as profile
        join player on player.id = profile.player_id
        join game on game.id = profile.game_id
        left join (
            select
                game_id,
                coalesce(
                    json_agg(
                        json_build_object(
                            'ilk', ilk,
                            'label', label,
                            'key', key,
                            'icon', icon,
                            'required', required,
                            'domain', domain,
                            'options', options
                        )
                        order by ordinal
                    )
                    filter (where key is not null),
                    '[]'
                )
                as "fields"
            from (
                select
                    field.id,
                    field.game_id,
                    field.ilk,
                    field.label,
                    field.key,
                    field.icon,
                    field.ordinal,
                    field.required,
                    field.domain,
                    json_agg(
                        json_build_object(
                            'key', field_option.key,
                            'label', field_option.label
                        )
                        order by field_option.ordinal
                    )
                    filter (where field_option.id is not null)
                    as options
                from field
                    left join field_option on field_option.field_id = field.id
                group by
                    field.id
                ) as field
            group by game_id
        ) as fields
            on fields.game_id = game.id
        left join (
            select
                profile.id as profile_id,
                coalesce(
                    json_agg(json_build_object(
                        'fieldKey', profile_value.key,
                        case
                            when profile_value.ilk = 1 then 'url'
                            when profile_value.ilk = 2 then 'optionKey'
                            when profile_value.ilk = 3 then 'optionKeys'
                        end,
                        case
                            when profile_value.ilk = 1 then url
                            when profile_value.ilk = 2 then single
                            when profile_value.ilk = 3 then multi
                        end
                    )) filter (where profile_value.field_value_id is not null),
                    '[]'
                ) as field_values
            from (
                select
                    field_value.player_profile_id,
                    field.key,
                    field.ilk,
                    field_value.id as field_value_id,
                    to_json(field_value.url) as url,
                    to_json(single.key) as single,
                    json_agg(multi.key) as multi
                from
                    player_profile_field_value as field_value
                join field
                    on field.id = field_value.field_id
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
            ) as profile_value
            join player_profile as profile on profile.id = profile_value.player_profile_id
            group by profile.id
        ) as field_values
            on field_values.profile_id = profile.id
    where lower(player.nickname) = lower($1)
    order by profile.updated desc;
    """

loadProfiles
    :: forall errors
    .  Pool
    -> Nickname
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool nickname = do
    result
        <- pool
        #  query queryString (nickname : [])
        #  label (SProxy :: SProxy "databaseError")
    profiles
        <- rows result
        #  traverse read
        #  labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure profiles
