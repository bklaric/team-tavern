module TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues) where

import Prelude

import Async (Async)
import Async as Async
import Postgres.Client (Client)
import Postgres.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryNone)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateFieldValues (FieldId, FieldValue(..), FieldValueId, FieldValueType(..), OptionId)

type ProfileId = Int

-- Insert related field value option rows.

insertValueOptionString :: Query
insertValueOptionString = Query """
    insert into player_profile_field_value_option (player_profile_field_value_id, field_option_id)
    values ($1, $2);
    """

insertMultiValueOption :: ∀ errors.
    Client -> FieldValueId -> OptionId -> Async (InternalTerror_ errors) Unit
insertMultiValueOption client fieldValueId optionId =
    queryNone client insertValueOptionString (fieldValueId :| optionId)

-- Insert field value row.

insertValueString :: Query
insertValueString = Query """
    insert into player_profile_field_value (player_profile_id, field_id)
    values ($1, $2)
    returning player_profile_field_value.id as "fieldValueId";
    """

insertValue :: ∀ errors.
    Client -> ProfileId -> FieldId -> Array OptionId -> Async (InternalTerror_ errors) Unit
insertValue client profileId fieldId optionIds = do
    -- Insert field value row.
    { fieldValueId } :: { fieldValueId :: FieldValueId } <-
        queryFirstInternal client insertValueString (profileId :| fieldId)

    -- Insert field value option rows.
    Async.foreach optionIds $ insertMultiValueOption client fieldValueId

-- Insert field value and related field value option rows.

addFieldValues :: ∀ errors.
    Client -> ProfileId -> Array FieldValue -> Async (InternalTerror_ errors) Unit
addFieldValues client profileId fieldValues =
    Async.foreach fieldValues \(FieldValue fieldId fieldValueType) ->
        case fieldValueType of
        Single optionId -> insertValue client profileId fieldId [optionId]
        Multi optionIds -> insertValue client profileId fieldId optionIds
