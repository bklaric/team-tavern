module TeamTavern.Server.Profile.Infrastructure.LoadFieldAndOptionIds (FieldAndOptionIds, loadFieldAndOptionIds) where

import Prelude

import Async (Async)
import Data.String (joinWith)
import Postgres.Query (class Querier, Query(..))
import TeamTavern.Routes.Shared.Filters (Field)
import TeamTavern.Routes.Shared.Types (Handle)
import TeamTavern.Server.Infrastructure.Postgres (prepareString, queryMany_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

type FieldAndOptionIds = { fieldId :: Int, optionIds :: Array Int }

queryStringFields :: Array Field -> String
queryStringFields fields = let
    stringOption optionKey = "field_option.key = " <> prepareString optionKey
    stringOptions optionKeys = (optionKeys <#> stringOption # joinWith " or ")
    stringField {fieldKey, optionKeys} =
        "(field.key = " <> prepareString fieldKey
        <> " and (" <> stringOptions optionKeys <> "))"
    stringFields = fields <#> stringField # joinWith " or "
    in " and (" <> stringFields <> ")"

queryString :: Handle -> Array Field -> Query
queryString handle fields = Query $ """
    select field.id as "fieldId", array_agg(field_option.id) as "optionIds"
    from field
    join field_option on field_option.field_id = field.id
    join game on game.id = field.game_id
    where game.handle = """ <> prepareString handle <> """
    """ <> queryStringFields fields <> """
    group by field.id"""

loadFieldAndOptionIds
    :: ∀ errors querier
    .  Querier querier
    => querier
    -> Handle
    -> Array Field
    -> Async (InternalTerror_ errors) (Array FieldAndOptionIds)
loadFieldAndOptionIds _ _ [] = pure []
loadFieldAndOptionIds client handle fields =
    queryMany_ client (queryString handle fields)
