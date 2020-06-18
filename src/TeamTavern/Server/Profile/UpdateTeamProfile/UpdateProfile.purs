module TeamTavern.Server.Profile.UpdateTeamProfile.UpdateProfile
    (UpdateProfileError, updateProfile) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Nullable (toNullable)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimespan (nullableTimeFrom, nullableTimeTo)
import TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateAgeSpan (nullableAgeFrom, nullableAgeTo)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Handle)

type UpdateProfileError errors = Variant
    ( databaseError :: Error
    , nothingInserted ::
        { cookieInfo :: CookieInfo
        , handle :: Handle
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

-- Update profile row.

updateProfileString :: Query
updateProfileString = Query """
    update team_profile
    set summary = $3,
        age_from = $4,
        age_to = $5,
        languages = $6,
        countries = $7,
        timezone = $8,
        weekday_from = $9,
        weekday_to = $10,
        weekend_from = $11,
        weekend_to = $12,
        has_microphone = $13,
        new_or_returning = $14,
        updated = now()
    from game
    where team_profile.player_id = $1
        and game.handle = $2
        and game.id = team_profile.game_id
    returning team_profile.id as "profileId";
    """

updateProfileParameters ::
    CookieInfo -> Handle -> Profile -> Array QueryParameter
updateProfileParameters { id } handle profile =
    id
    : handle
    : profile.summary
    : nullableAgeFrom profile.ageSpan
    : nullableAgeTo profile.ageSpan
    : profile.languages
    : profile.countries
    : toNullable profile.timezone
    : nullableTimeFrom profile.onlineWeekday
    : nullableTimeTo profile.onlineWeekday
    : nullableTimeFrom profile.onlineWeekend
    : nullableTimeTo profile.onlineWeekend
    : profile.hasMicrophone
    :| profile.newOrReturning

updateProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Profile
    -> Async (UpdateProfileError errors) ProfileId
updateProfile' client cookieInfo handle profile = do
    result <- client
        # query updateProfileString
            (updateProfileParameters cookieInfo handle profile)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- rows result
        # head
        # Async.note (inj
            (SProxy :: SProxy "nothingInserted") { cookieInfo, handle })
        >>= (read >>> labelMap
            (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

-- Delete field value rows.

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from team_profile_field_value
    where team_profile_id = $1;
    """

deleteFieldValues :: forall errors.
    Client -> ProfileId -> Async (UpdateProfileError errors) Unit
deleteFieldValues client profileId =
    client
    # execute deleteFieldValuesString (profileId : [])
    # label (SProxy :: SProxy "databaseError")

updateProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Profile
    -> Async (UpdateProfileError errors) Unit
updateProfile client cookieInfo handle profile = do
    -- Update profile row.
    profileId <- updateProfile' client cookieInfo handle profile

    -- Delete all existing field values.
    deleteFieldValues client profileId

    -- Insert new field values.
    addFieldValues client profileId profile.fieldValues
