module TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues) where

import Prelude

import Async (Async)
import Async as Async
import Postgres.Client (Client)
import Postgres.Query (Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryNone)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (FieldId, FieldValue(..), FieldValueId, FieldValueType(..), OptionId)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)

type ProfileId = Int

-- Insert url field value row.

insertUrlValueString :: Query
insertUrlValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, $3, null);
    """

insertUrlValue :: forall errors.
    Client -> ProfileId -> FieldId -> Url -> Async (InternalError errors) Unit
insertUrlValue client profileId fieldId url =
    queryNone client insertUrlValueString (profileId : fieldId :| url)

-- Insert single select value row.

insertSingleValueString :: Query
insertSingleValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, null, $3);
    """

insertSingleValue :: forall errors.
    Client -> ProfileId -> FieldId -> OptionId -> Async (InternalError errors) Unit
insertSingleValue client profileId fieldId optionId =
    queryNone client insertSingleValueString (profileId : fieldId :| optionId)

-- Insert multiselect value row and related field value option rows.

insertMultiValueOptionString :: Query
insertMultiValueOptionString = Query """
    insert into player_profile_field_value_option (player_profile_field_value_id, field_option_id)
    values ($1, $2);
    """

insertMultiValueOption :: forall errors.
    Client -> FieldValueId -> OptionId -> Async (InternalError errors) Unit
insertMultiValueOption client fieldValueId optionId =
    queryNone client insertMultiValueOptionString (fieldValueId :| optionId)

insertMultiValueString :: Query
insertMultiValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id, url, field_option_id)
    values ($1, $2, null, null)
    returning player_profile_field_value.id as "fieldValueId";
    """

insertMultiValue :: forall errors.
    Client -> ProfileId -> FieldId -> Array OptionId -> Async (InternalError errors) Unit
insertMultiValue client profileId fieldId optionIds = do
    -- Insert field value row.
    { fieldValueId } :: { fieldValueId :: FieldValueId } <-
        queryFirstInternal client insertMultiValueString (profileId :| fieldId)

    -- Insert field value option rows.
    Async.foreach optionIds $ insertMultiValueOption client fieldValueId

-- Insert field value rows.

addFieldValues :: forall errors.
    Client -> ProfileId -> Array FieldValue -> Async (InternalError errors) Unit
addFieldValues client profileId fieldValues =
    Async.foreach fieldValues \(FieldValue fieldId fieldValueType) ->
        case fieldValueType of
        Url url -> insertUrlValue client profileId fieldId url
        Single optionId -> insertSingleValue client profileId fieldId optionId
        Multi optionIds -> insertMultiValue client profileId fieldId optionIds
