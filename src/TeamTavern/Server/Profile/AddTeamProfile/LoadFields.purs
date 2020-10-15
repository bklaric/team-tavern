module TeamTavern.Server.Profile.AddTeamProfile.LoadFields (Option, Field, loadFields) where

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

type Option =
    { id :: Int
    , key :: String
    , label :: String
    }

type Field =
    { id :: Int
    , key :: String
    , options :: Array Option
    }

queryString :: Query
queryString = Query """
    select
        field.id,
        field.key,
        json_agg(
            json_build_object(
                'id', field_option.id,
                'key', field_option.key,
                'label', field_option.label
            )
            order by field_option.id
        )
        filter (where field_option.id is not null)
        as options
    from field
        join game on game.id = field.game_id
        left join field_option on field_option.field_id = field.id
    where game.handle = $1
        and (field.ilk = 2 or field.ilk = 3)
    group by field.id
    """

loadFields :: forall errors. Client -> String -> Async (InternalError errors) (Array Field)
loadFields client handle = queryMany client queryString (handle : [])
