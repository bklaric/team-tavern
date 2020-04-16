module TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.String as String
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
import TeamTavern.Server.Profile.Routes (Nickname, Timezone)

type LoadProfilesResult =
    { handle :: String
    , title :: String
    , age :: { from :: Maybe Int, to :: Maybe Int }
    , countries :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , fields :: Array
        { key :: String
        , label :: String
        , icon :: String
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , summary :: Array String
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

prepareString :: String -> String
prepareString stringValue =
       "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") stringValue)
    <> "'"

timezoneAdjustedTime :: Timezone -> String -> String
timezoneAdjustedTime timezone timeColumn =
    """((current_date || ' ' || """ <> timeColumn <> """ || ' ' || profile.timezone)::timestamptz
    at time zone """ <> prepareString timezone <> """)::time"""

queryString :: Timezone -> Query
queryString timezone = Query $ """
    select
        game.handle,
        game.title,
            json_build_object(
                'from', profile.age_from,
                'to', profile.age_to
            ) as age,
            profile.countries,
            profile.languages,
            profile.has_microphone as "hasMicrophone",
            case
                when profile.weekday_from is not null and profile.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> timezoneAdjustedTime timezone "profile.weekday_from" <> """, 'HH24:MI'),
                    'to', to_char(""" <> timezoneAdjustedTime timezone "profile.weekday_to" <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when profile.weekend_from is not null and profile.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> timezoneAdjustedTime timezone "profile.weekend_from" <> """, 'HH24:MI'),
                    'to', to_char(""" <> timezoneAdjustedTime timezone "profile.weekend_to" <> """, 'HH24:MI')
                )
            end as "weekendOnline",
        coalesce(fields.fields, '[]') as "fields",
        coalesce(field_values.field_values, '[]') as "fieldValues",
        profile.summary,
        profile.updated::text,
        extract(epoch from (now() - updated)) as "updatedSeconds"
    from team_profile as profile
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
                where
                    field.ilk = 2 or field.ilk = 3
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
                        'optionKeys', multi
                    )) filter (where profile_value.field_value_id is not null),
                    '[]'
                ) as field_values
            from (
                select
                    field_value.team_profile_id,
                    field.key,
                    field.ilk,
                    field_value.id as field_value_id,
                    json_agg(multi.key) as multi
                from
                    team_profile_field_value as field_value
                join field
                    on field.id = field_value.field_id
                left join team_profile_field_value_option as field_value_option
                    on field_value_option.team_profile_field_value_id = field_value.id
                left join field_option as multi
                    on multi.id = field_value_option.field_option_id
                group by
                    field.id,
                    field_value.id
            ) as profile_value
            join team_profile as profile on profile.id = profile_value.team_profile_id
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
    -> Timezone
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool nickname timezone = do
    result
        <- pool
        #  query (queryString timezone) (nickname : [])
        #  label (SProxy :: SProxy "databaseError")
    profiles
        <- rows result
        #  traverse read
        #  labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure profiles
