module TeamTavern.Server.Profile.UpdatePlayerProfile.UpdateProfile (updateProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import Yoga.JSON (writeImpl)

-- Update profile row.

updateProfileString :: Query
updateProfileString = Query """
    update player_profile
    set platform = $3,
        new_or_returning = $4,
        about = $5,
        ambitions = $6,
        updated = now()
    from player, game
    where lower(player.nickname) = lower($1)
    and player.id = player_profile.player_id
    and game.id = player_profile.game_id
    and game.handle = $2
    returning player_profile.id as "profileId";
    """

updateProfileParameters ::
    AddPlayerProfile.RouteParams -> Profile -> Array QueryParameter
updateProfileParameters
    { nickname, handle }
    { platform, newOrReturning, about, ambitions } =
    nickname : handle : writeImpl platform : newOrReturning : about :| ambitions

updateProfile' :: ∀ errors.
    Client -> AddPlayerProfile.RouteParams -> Profile -> Async (InternalTerror_ errors) ProfileId
updateProfile' client identifiers profile = do
    { profileId } :: { profileId :: Int } <-
        queryFirstInternal client updateProfileString
        (updateProfileParameters identifiers profile)
    pure profileId

-- Delete field value rows.

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from player_profile_field_value
    where player_profile_id = $1;
    """

deleteFieldValues :: ∀ errors.
    Client -> ProfileId -> Async (InternalTerror_ errors) Unit
deleteFieldValues client profileId =
    queryNone client deleteFieldValuesString (profileId : [])

updateProfile
    :: ∀ errors
    .  Client
    -> AddPlayerProfile.RouteParams
    -> Profile
    -> Async (InternalTerror_ errors) Unit
updateProfile client identifiers profile @ { fieldValues } = do
    -- Update profile row.
    profileId <- updateProfile' client identifiers profile

    -- Delete all existing field values.
    deleteFieldValues client profileId

    -- Insert new field values.
    addFieldValues client profileId fieldValues
