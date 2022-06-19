module TeamTavern.Server.Profile.UpdatePlayerProfile.UpdateProfile
    (UpdateProfileError, updateProfile) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result, rows)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeImpl)
import Yoga.JSON.Async (read)

type UpdateProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: AddPlayerProfile.RouteParams
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
    , internal :: Array String
    | errors )

-- Update profile row.

updateProfileString :: Query
updateProfileString = Query """
    update player_profile
    set platform = $5,
        new_or_returning = $6,
        about = $7,
        ambitions = $8,
        updated = now()
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and player.id = player_profile.player_id
    and game.id = player_profile.game_id
    and lower(player.nickname) = lower($3)
    and game.handle = $4
    returning player_profile.id as "profileId";
    """

updateProfileParameters :: CookieInfo -> AddPlayerProfile.RouteParams -> Profile -> Array QueryParameter
updateProfileParameters { id, token } { nickname, handle }
    { platform, newOrReturning, about, ambitions } =
    id : token : nickname : handle : writeImpl platform : newOrReturning : about :| ambitions

updateProfile' :: forall errors.
    Client -> CookieInfo -> AddPlayerProfile.RouteParams -> Profile -> Async (UpdateProfileError errors) ProfileId
updateProfile' client cookieInfo identifiers profile = do
    result <- client
        # query updateProfileString
            (updateProfileParameters cookieInfo identifiers profile)
        # label (Proxy :: _ "databaseError")
    { profileId } :: { profileId :: Int } <- rows result
        # head
        # Async.note (inj
            (Proxy :: _ "notAuthorized") { cookieInfo, identifiers })
        >>= (read >>> labelMap
            (Proxy :: _ "unreadableProfileId") { result, errors: _ })
    pure profileId

-- Delete field value rows.

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from player_profile_field_value
    where player_profile_id = $1;
    """

deleteFieldValues :: forall errors.
    Client -> ProfileId -> Async (UpdateProfileError errors) Unit
deleteFieldValues client profileId =
    client
    # execute deleteFieldValuesString (profileId : [])
    # label (Proxy :: _ "databaseError")

updateProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> AddPlayerProfile.RouteParams
    -> Profile
    -> Async (UpdateProfileError errors) Unit
updateProfile client cookieInfo identifiers profile @ { fieldValues } = do
    -- Update profile row.
    profileId <- updateProfile' client cookieInfo identifiers profile

    -- Delete all existing field values.
    deleteFieldValues client profileId

    -- Insert new field values.
    addFieldValues client profileId fieldValues
