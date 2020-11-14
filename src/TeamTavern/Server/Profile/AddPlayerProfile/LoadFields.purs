module TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (Option, Field, loadFields) where

import Async (Async)
import Data.Maybe (Maybe)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

type Handle = String

type Option =
    { id :: Int
    , key :: String
    , domain :: Maybe String
    }

type Field =
    { id :: Int
    , ilk :: Int
    , key :: String
    , required :: Boolean
    , domain :: Maybe String
    , options :: Maybe (Array Option)
    }

queryString :: Query
queryString = Query """
    select
        field.id,
        field.ilk,
        field.key,
        field.required,
        field.domain,
        json_agg(
            json_build_object(
                'id', field_option.id,
                'key', field_option.key
            )
            order by field_option.id
        )
        filter (where field_option.id is not null)
        as options
    from field
        join game on game.id = field.game_id
        left join field_option on field_option.field_id = field.id
    where game.handle = $1
    group by field.id
    """

loadFields :: forall querier errors. Querier querier =>
    querier -> Handle -> Async (InternalError errors) (Array Field)
loadFields client handle = queryMany client queryString (handle : [])
