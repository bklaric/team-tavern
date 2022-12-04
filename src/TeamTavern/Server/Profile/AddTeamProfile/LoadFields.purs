module TeamTavern.Server.Profile.AddTeamProfile.LoadFields (Option, Field, Game, loadFields) where

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), (:))
import TeamTavern.Routes.Shared.Platform (Platforms)
import TeamTavern.Server.Infrastructure.Error (InternalTerror_)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)

type Option =
    { id :: Int
    , key :: String
    }

type Field =
    { id :: Int
    , key :: String
    , options :: Array Option
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
                    'key', field.key,
                    'options', field.options
                ) order by field.ordinal
            ) filter (where field.id is not null),
            '[]'
        ) as fields
    from game
        left join (
            select
                field.*,
                coalesce(
                    json_agg(
                        json_build_object(
                            'id', field_option.id,
                            'key', field_option.key
                        ) order by field_option.id
                    ) filter (where field_option.id is not null),
                    '[]'
                ) as options
            from field left join field_option on field_option.field_id = field.id
            where field.ilk = 2 or field.ilk = 3
            group by field.id
        ) as field on field.game_id = game.id
    where game.handle = $1
    group by game.id;
    """

loadFields :: forall errors. Client -> String -> Async (InternalTerror errors) Game
loadFields client handle = queryFirstInternal client queryString (handle : [])
