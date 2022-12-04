module TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (Option, Field, Game, loadFields) where

import Async (Async)
import Data.Maybe (Maybe)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Shared.Platform (Platforms)
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)

type Handle = String

type Option =
    { id :: Int
    , key :: String
    }

type Field =
    { id :: Int
    , ilk :: Int
    , key :: String
    , domain :: Maybe String
    , options :: Maybe (Array Option)
    }

type Game =
    { platforms :: Platforms
    , fields :: Array Field
    }

queryString :: Query
queryString = Query """
    select
        json_build_object(
            'head', game.platforms[1],
            'tail', game.platforms[2:]
        ) as platforms,
        coalesce(
            json_agg(
                json_build_object(
                    'id', field.id,
                    'ilk', field.ilk,
                    'key', field.key,
                    'domain', field.domain,
                    'options', field.options
                ) order by field.ordinal
            ) filter (where field.id is not null),
            '[]'
        ) as fields
    from game
        left join (
            select
                field.*,
                json_agg(
                    json_build_object(
                        'id', field_option.id,
                        'key', field_option.key
                    ) order by field_option.ordinal
                ) filter (where field_option.id is not null) as options
            from field left join field_option on field_option.field_id = field.id
            group by field.id
        ) as field on field.game_id = game.id
    where game.handle = $1
    group by game.id;
    """

loadFields :: forall querier errors. Querier querier =>
    querier -> Handle -> Async (InternalError errors) Game
loadFields client handle = queryFirstInternal client queryString (handle : [])
