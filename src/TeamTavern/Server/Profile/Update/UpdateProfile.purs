module TeamTavern.Server.Profile.Update.UpdateProfile
    (UpdateProfileError, updateProfile) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.Infrastructure.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.Infrastructure.ValidateProfile (Profile(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateSummary (Summary)
import TeamTavern.Server.Profile.Routes (Identifiers)

type UpdateProfileError errors = Variant
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

-- Update profile row.

updateProfileString :: Query
updateProfileString = Query """
    update profile
    set summary = $6, updated = now()
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and player.id = profile.player_id
    and game.id = profile.game_id
    and lower(player.nickname) = lower($3)
    and game.handle = $4
    and profile.type = $5
    returning profile.id as "profileId";
    """

updateProfileParameters ::
    CookieInfo -> Identifiers -> Int -> Summary -> Array QueryParameter
updateProfileParameters { id, token } { nickname, handle } profileType summary =
    id : token : nickname : handle : profileType :| summary

updateProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Int
    -> Summary
    -> Async (UpdateProfileError errors) ProfileId
updateProfile' client cookieInfo identifiers profileType summary = do
    result <- client
        # query updateProfileString
            (updateProfileParameters cookieInfo identifiers profileType summary)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- rows result
        # head
        # Async.note (inj
            (SProxy :: SProxy "notAuthorized") { cookieInfo, identifiers })
        >>= (read >>> labelMap
            (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    pure profileId

-- Delete field value rows.

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from field_value
    where profile_id = $1;
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
    -> Identifiers
    -> Profile
    -> Async (UpdateProfileError errors) Unit
updateProfile client cookieInfo identifiers (Profile profileType summary fieldValues) = do
    -- Update profile row.
    profileId <- updateProfile' client cookieInfo identifiers profileType summary

    -- Delete all existing field values.
    deleteFieldValues client profileId

    -- Insert new field values.
    addFieldValues client profileId fieldValues
