module TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues (ProfileId, addFieldValues) where

import Prelude

import Async (Async)
import Async as Async
import Postgres.Client (Client)
import Postgres.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryNone)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues (FieldValue(..), OptionId)

type ProfileId = Int

type FieldValueId = Int

-- Insert field value row and related field value option rows.

insertFieldValueOptionString :: Query
insertFieldValueOptionString = Query """
    insert into team_profile_field_value_option
        (team_profile_field_value_id, field_option_id)
    values ($1, $2);
    """

insertFieldValueOption :: forall errors.
    Client -> FieldValueId -> OptionId -> Async (InternalError errors) Unit
insertFieldValueOption client fieldValueId optionId =
    queryNone client insertFieldValueOptionString (fieldValueId :| optionId)

insertFieldValueString :: Query
insertFieldValueString = Query """
    insert into team_profile_field_value (team_profile_id, field_id)
    values ($1, $2)
    returning team_profile_field_value.id as "fieldValueId";
    """

insertFieldValue :: forall errors.
    Client -> ProfileId -> FieldValue -> Async (InternalError errors) Unit
insertFieldValue client profileId (FieldValue fieldId optionIds) = do
    -- Insert field value row.
    ({ fieldValueId } :: { fieldValueId :: Int }) <-
        queryFirstInternal client insertFieldValueString (profileId :| fieldId)

    -- Insert field value option rows.
    Async.foreach optionIds $ insertFieldValueOption client fieldValueId

addFieldValues
    :: forall errors
    .  Client
    -> ProfileId
    -> Array FieldValue
    -> Async (InternalError errors) Unit
addFieldValues client profileId fieldValues =
    Async.foreach fieldValues $ insertFieldValue client profileId
