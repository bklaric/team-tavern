module TeamTavern.Server.Profile.Infrastructure.AddFieldValues where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label)
import Data.Bifunctor.Label as Label
import Data.List (List)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query as Postgres
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:), (:|))
import Postgres.Result (Result)
import Postgres.Result as Result
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Profile.Infrastructure.ValidateFieldValues (FieldId, FieldValue(..), FieldValueId, FieldValueType(..), OptionId)

type ProfileId = Int

type AddFieldValuesError errors = Variant
    ( databaseError :: Error
    , emptyResult ::
        { result :: Result
        }
    , unreadableFieldValueId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

-- Insert url field value row.

insertUrlValueString :: Query
insertUrlValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, $3, null);
    """

insertUrlValue :: forall errors.
    Client -> ProfileId -> FieldId -> Url -> Async (AddFieldValuesError errors) Unit
insertUrlValue client profileId fieldId url =
    client
    # Postgres.execute insertUrlValueString (profileId : fieldId :| url)
    # label (SProxy :: SProxy "databaseError")

-- Insert single select value row.

insertSingleValueString :: Query
insertSingleValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, null, $3);
    """

insertSingleValue :: forall errors.
    Client -> ProfileId -> FieldId -> OptionId -> Async (AddFieldValuesError errors) Unit
insertSingleValue client profileId fieldId optionId =
    client
    # Postgres.execute insertSingleValueString (profileId : fieldId :| optionId)
    # label (SProxy :: SProxy "databaseError")

-- Insert multiselect value row and related field value option rows.

insertMultiValueOptionString :: Query
insertMultiValueOptionString = Query """
    insert into player_profile_field_value_option (player_profile_field_value_id, field_option_id)
    values ($1, $2);
    """

insertMultiValueOption :: forall errors.
    Client -> FieldValueId -> OptionId -> Async (AddFieldValuesError errors) Unit
insertMultiValueOption client fieldValueId optionId =
    client
    # Postgres.execute insertMultiValueOptionString (fieldValueId :| optionId)
    # label (SProxy :: SProxy "databaseError")

insertMultiValueString :: Query
insertMultiValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, null, null)
    returning player_profile_field_value.id as "fieldValueId";
    """

insertMultiValue :: forall errors.
    Client -> ProfileId -> FieldId -> Array OptionId -> Async (AddFieldValuesError errors) Unit
insertMultiValue client profileId fieldId optionIds = do
    -- Insert field value row.
    result <- client
        # Postgres.query insertMultiValueString (profileId :| fieldId)
        # Label.label (SProxy :: SProxy "databaseError")
    { fieldValueId } :: { fieldValueId :: FieldValueId } <- Result.rows result
        # head
        # Async.note (inj (SProxy :: SProxy "emptyResult") { result })
        >>= (read >>> Label.labelMap
            (SProxy :: SProxy "unreadableFieldValueId") { result, errors: _ })

    -- Insert field value option rows.
    Async.foreach optionIds $ insertMultiValueOption client fieldValueId

-- Insert field value rows.

addFieldValues
    :: forall errors
    .  Client
    -> ProfileId
    -> List FieldValue
    -> Async (AddFieldValuesError errors) Unit
addFieldValues client profileId fieldValues =
    Async.foreach fieldValues \(FieldValue fieldId fieldValueType) ->
        case fieldValueType of
        Url url -> insertUrlValue client profileId fieldId url
        Single optionId -> insertSingleValue client profileId fieldId optionId
        Multi optionIds -> insertMultiValue client profileId fieldId optionIds
