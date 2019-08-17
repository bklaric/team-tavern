module TeamTavern.Server.Profile.Infrastructure.LoadFields
    (FieldDto, LoadFieldsError, loadFields) where

import Prelude

import Async (Async, left, right)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Profile.Domain.FieldValue (Field, createField)

type FieldDto =
    { id :: Int
    , type :: Int
    , data :: { options :: Maybe (Array { id :: Int }) }
    }

type LoadFieldsError errors = Variant
    ( databaseError :: Error
    , unreadableFieldDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , invalidFieldModels ::
        { dtos :: Array FieldDto }
    | errors )

queryString :: Query
queryString = Query """
    select field.id, field.type, field.data
    from field
    join game on game.id = field.game_id
    where game.handle = $1;
    """

queryParameters :: Handle -> Array QueryParameter
queryParameters handle = handle : []

loadFields
    :: forall errors
    .  Client
    -> Handle
    -> Async (LoadFieldsError errors) (Array Field)
loadFields client handle = do
    result <- client
        # query queryString (queryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    dtos :: Array FieldDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableFieldDtos")
            { result, errors: _ }
    let dtos' = dtos <#> \dto ->
            { id: dto.id, type: dto.type, options: dto.data.options }
    case traverse createField dtos' of
        Just fields -> right fields
        Nothing -> left $ inj (SProxy :: SProxy "invalidFieldModels") { dtos }
