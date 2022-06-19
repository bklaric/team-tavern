module TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size as Size
import TeamTavern.Routes.Shared.Types (Handle)
import TeamTavern.Server.Infrastructure.Error (ChangeSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotAuthorized)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Profile.AddTeamProfile.AddFieldValues (addFieldValues)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (Profile)

queryString :: Query
queryString = Query """
    insert into team_profile
        ( team_id
        , game_id
        , size
        , platforms
        , new_or_returning
        , about
        , ambitions
        )
    select team.id, game.id, $4, $5, $6, $7, $8
    from player, team, game
    where player.id = $1
        and team.handle = $2
        and game.handle = $3
        and team.owner_id = player.id
    returning team_profile.id as "profileId"
    """

queryParameters :: Id -> Handle -> Handle -> Profile -> Array QueryParameter
queryParameters id teamHandle gameHandle profile =
    id
    : teamHandle
    : gameHandle
    : (Size.toString profile.size)
    : (profile.platforms <#> Platform.toString)
    : profile.newOrReturning
    : profile.about
    :| profile.ambitions

addProfile'
    :: forall errors
    .  Client
    -> Id
    -> Handle
    -> Handle
    -> Profile
    -> Async (ChangeSingleError errors) { profileId :: Int }
addProfile' client id teamHandle gameHandle profile = do
    queryFirstNotAuthorized client queryString
        (queryParameters id teamHandle gameHandle profile)

addProfile
    :: forall errors
    .  Client
    -> Id
    -> Handle
    -> Handle
    -> Profile
    -> Async (ChangeSingleError errors) Int
addProfile client id teamHandle gameHandle profile = do
    { profileId } <- addProfile' client id teamHandle gameHandle profile
    addFieldValues client profileId profile.fieldValues
    pure profileId
