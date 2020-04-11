module TeamTavern.Server.Profile.AddGameTeam.LoadFields
    (Option, Field, LoadFieldsError, loadFields) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)

type Handle = String

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

type LoadFieldsError errors = Variant
    ( databaseError :: Error
    , unreadableFields ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

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

loadFields :: forall errors.
    Client -> Handle -> Async (LoadFieldsError errors) (Array Field)
loadFields client handle = do
    result <- client
        # query queryString (handle : [])
        # label (SProxy :: SProxy "databaseError")
    fields <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableFields") { result, errors: _ }
    pure fields
