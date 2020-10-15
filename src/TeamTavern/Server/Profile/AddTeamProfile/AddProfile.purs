module TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Infrastructure.Error (ChangeSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotAuthorized)
import TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues (addFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Handle)

queryString :: Query
queryString = Query """
    insert into team_profile
        ( team_id
        , game_id
        , new_or_returning
        , summary
        )
    select team.id, game.id, $4, $5
    from player, team, game
    where player.id = $1 and team.handle = $2 and game.handle = $3 and team.owner_id = player.id
    returning team_profile.id as "profileId"
    """

queryParameters :: CookieInfo -> Handle -> Handle -> Profile -> Array QueryParameter
queryParameters { id } teamHandle gameHandle profile =
    id
    : teamHandle
    : gameHandle
    : profile.newOrReturning
    :| profile.ambitions

addProfile'
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Handle
    -> Profile
    -> Async (ChangeSingleError errors) { profileId :: Int }
addProfile' client cookieInfo teamHandle gameHandle profile = do
    queryFirstNotAuthorized client queryString
        (queryParameters cookieInfo teamHandle gameHandle profile)

addProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Handle
    -> Handle
    -> Profile
    -> Async (ChangeSingleError errors) Unit
addProfile client cookieInfo teamHandle gameHandle profile = do
    { profileId } <- addProfile' client cookieInfo teamHandle gameHandle profile
    addFieldValues client profileId profile.fieldValues
