module TeamTavern.Server.Profile.UpdateTeamProfile.UpdateProfile (updateProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Error (InternalError, ChangeSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotAuthorized, queryNone)
import TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Handle)

-- Update profile row.

updateProfileString :: Query
updateProfileString = Query """
    update team_profile
    set new_or_returning = $4,
        summary = $5,
        updated = now()
    from player, team, game
    where player.id = $1
        and team.handle = $2
        and game.handle = $3
        and team_profile.team_id = team.id
        and team_profile.game_id = game.id
        and team.owner_id = player.id
    returning team_profile.id as "profileId"
    """

updateProfileParameters :: CookieInfo -> Handle -> Handle -> Profile -> Array QueryParameter
updateProfileParameters { id } teamHandle gameHandle profile =
    id : teamHandle : gameHandle : profile.newOrReturning :| profile.ambitions

updateProfile' :: forall errors.
    Client -> CookieInfo -> Handle -> Handle -> Profile -> Async (ChangeSingleError errors) { profileId :: Int }
updateProfile' client cookieInfo teamHandle gameHandle profile =
    queryFirstNotAuthorized client updateProfileString
        (updateProfileParameters cookieInfo teamHandle gameHandle profile)

-- Delete field value rows.

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from team_profile_field_value
    where team_profile_id = $1;
    """

deleteFieldValues :: forall errors. Client -> ProfileId -> Async (InternalError errors) Unit
deleteFieldValues client profileId = queryNone client deleteFieldValuesString (profileId : [])

updateProfile :: forall errors.
    Client -> CookieInfo -> Handle -> Handle -> Profile -> Async (ChangeSingleError errors) Unit
updateProfile client cookieInfo teamHandle gameHandle profile = do
    -- Update profile row.
    { profileId } <- updateProfile' client cookieInfo teamHandle gameHandle profile

    -- Delete all existing field values.
    deleteFieldValues client profileId

    -- Insert new field values.
    addFieldValues client profileId profile.fieldValues
