module TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues
    (ProfileId, AddFieldValuesError, addFieldValues) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label)
import Data.Bifunctor.Label as Label
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query as Postgres
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (Result)
import Postgres.Result as Result
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateFieldValues (FieldValue(..), OptionId)

type ProfileId = Int

type FieldValueId = Int

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

-- Insert field value row and related field value option rows.

insertFieldValueOptionString :: Query
insertFieldValueOptionString = Query """
    insert into team_profile_field_value_option
        (team_profile_field_value_id, field_option_id)
    values ($1, $2);
    """

insertFieldValueOption
    :: forall errors
    .  Client
    -> FieldValueId
    -> OptionId
    -> Async (AddFieldValuesError errors) Unit
insertFieldValueOption client fieldValueId optionId =
    client
    # Postgres.execute insertFieldValueOptionString (fieldValueId :| optionId)
    # label (SProxy :: SProxy "databaseError")

insertFieldValueString :: Query
insertFieldValueString = Query """
    insert into team_profile_field_value (team_profile_id, field_id)
    values ($1, $2)
    returning team_profile_field_value.id as "fieldValueId";
    """

insertFieldValue :: forall errors.
    Client -> ProfileId -> FieldValue -> Async (AddFieldValuesError errors) Unit
insertFieldValue client profileId (FieldValue fieldId optionIds) = do
    -- Insert field value row.
    result <-
        client
        # Postgres.query insertFieldValueString (profileId :| fieldId)
        # Label.label (SProxy :: SProxy "databaseError")
    { fieldValueId } :: { fieldValueId :: FieldValueId } <-
        Result.rows result
        # head
        # Async.note (inj (SProxy :: SProxy "emptyResult") { result })
        >>= (read >>> Label.labelMap
            (SProxy :: SProxy "unreadableFieldValueId") { result, errors: _ })

    -- Insert field value option rows.
    Async.foreach optionIds $ insertFieldValueOption client fieldValueId

addFieldValues
    :: forall errors
    .  Client
    -> ProfileId
    -> Array FieldValue
    -> Async (AddFieldValuesError errors) Unit
addFieldValues client profileId fieldValues =
    Async.foreach fieldValues $ insertFieldValue client profileId
