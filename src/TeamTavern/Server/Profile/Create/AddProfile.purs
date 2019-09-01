module TeamTavern.Server.Profile.Create.AddProfile where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Bifunctor.Label as Label
import Data.List (List)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Async.Query as Postgres
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result)
import Postgres.Result as Result
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.Infrastructure.ValidateProfile (Profile(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary)
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url)
import TeamTavern.Server.Profile.Routes (Identifiers)
import TeamTvaern.Server.Profile.Infrastructure.ValidateFieldValues (FieldId, FieldValue(..), FieldValueId, FieldValueType(..), OptionId)

type ProfileId = Int

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: Identifiers
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , emptyResult ::
        { result :: Result
        }
    , unreadableFieldValueId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

-- Insert profile row.

insertProfileString :: Query
insertProfileString = Query """
    insert into profile (player_id, game_id, summary)
    select player.id, game.id, $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    and player.nickname = $4
    returning profile.id as "profileId";
    """

insertProfileParameters ::
    CookieInfo -> Identifiers -> Summary -> Array QueryParameter
insertProfileParameters { id, token } { handle, nickname } summary =
    id : token : handle : nickname :| summary

insertProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Summary
    -> Async (AddProfileError errors) ProfileId
insertProfile client cookieInfo identifiers summary = do
    result <- client
        # query insertProfileString (insertProfileParameters cookieInfo identifiers summary)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- Result.rows result
        # head
        # Async.note (inj (SProxy :: SProxy "notAuthorized") { cookieInfo, identifiers })
        >>= (read >>> labelMap (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

-- Insert url field value row.

insertUrlValueString :: Query
insertUrlValueString = Query """
    insert into field_value (profile_id, field_id, url, field_option_id)
    values ($1, $2, $3, null);
    """

insertUrlValue :: forall errors.
    Client -> ProfileId -> FieldId -> Url -> Async (AddProfileError errors) Unit
insertUrlValue client profileId fieldId url =
    client
    # Postgres.execute insertUrlValueString (profileId : fieldId :| url)
    # label (SProxy :: SProxy "databaseError")

-- Insert single select value row.

insertSingleValueString :: Query
insertSingleValueString = Query """
    insert into field_value (profile_id, field_id, url, field_option_id)
    values ($1, $2, null, $3);
    """

insertSingleValue :: forall errors.
    Client -> ProfileId -> FieldId -> OptionId -> Async (AddProfileError errors) Unit
insertSingleValue client profileId fieldId optionId =
    client
    # Postgres.execute insertSingleValueString (profileId : fieldId :| optionId)
    # label (SProxy :: SProxy "databaseError")

-- Insert multiselect value row and related field value option rows.

insertMultiValueOptionString :: Query
insertMultiValueOptionString = Query """
    insert into field_value_option (field_value_id, field_option_id)
    values ($1, $2);
    """

insertMultiValueOption :: forall errors.
    Client -> FieldValueId -> OptionId -> Async (AddProfileError errors) Unit
insertMultiValueOption client fieldValueId optionId =
    client
    # Postgres.execute insertMultiValueOptionString (fieldValueId :| optionId)
    # label (SProxy :: SProxy "databaseError")

insertMultiValueString :: Query
insertMultiValueString = Query """
    insert into field_value (profile_id, field_id, url, field_option_id)
    values ($1, $2, null, null)
    returning field_value.id as "fieldValueId";
    """

insertMultiValue :: forall errors.
    Client -> ProfileId -> FieldId -> Array OptionId -> Async (AddProfileError errors) Unit
insertMultiValue client profileId fieldId optionIds = do
    -- Insert field value row.
    result <- client
        # Postgres.query insertMultiValueString (profileId :| fieldId)
        # Label.label (SProxy :: SProxy "databaseError")
    { fieldValueId } :: { fieldValueId :: FieldValueId } <- Result.rows result
        # head
        # Async.note (inj (SProxy :: SProxy "emptyResult") { result })
        >>= (read >>> Label.labelMap (SProxy :: SProxy "unreadableFieldValueId") { result, errors: _ })

    -- Insert field value option rows.
    Async.foreach optionIds $ insertMultiValueOption client fieldValueId

-- Add profile with field values.

insertFieldValues :: forall errors.
    Client -> ProfileId -> List FieldValue -> Async (AddProfileError errors) Unit
insertFieldValues client profileId fieldValues =
    Async.foreach fieldValues \(FieldValue fieldId fieldValueType) ->
        case fieldValueType of
        Url url -> insertUrlValue client profileId fieldId url
        Single optionId -> insertSingleValue client profileId fieldId optionId
        Multi optionIds -> insertMultiValue client profileId fieldId optionIds

addProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Profile
    -> Async (AddProfileError errors) Unit
addProfile client cookieInfo identifiers (Profile summary fieldValues ) = do
    profileId <- insertProfile client cookieInfo identifiers summary
    insertFieldValues client profileId fieldValues
    pure unit
