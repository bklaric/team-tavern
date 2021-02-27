module TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Simple.JSON (writeImpl)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidatePlatformId as PlatformId
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Identifiers)

queryString :: Query
queryString = Query """
    insert into player_profile
        (player_id, game_id, platform, platform_id, ambitions, new_or_returning)
    select $1, game.id, $3, $4, $5, $6
    from game
    where game.handle = $2
    returning player_profile.id as "profileId";
    """

queryParameters :: Int -> Identifiers -> Profile -> Array QueryParameter
queryParameters playerId { handle, nickname }
    { platform, platformId, ambitions, newOrReturning } =
    playerId : handle : writeImpl platform : PlatformId.toString platformId
    : ambitions :| newOrReturning

addProfile' :: forall errors.
    Client -> Int -> Identifiers -> Profile -> Async (InternalError errors) ProfileId
addProfile' client playerId identifiers profile = do
    { profileId } :: { profileId :: Int } <- queryFirstInternal client queryString
        (queryParameters playerId identifiers profile)
    pure profileId

addProfile :: forall errors.
    Client -> Int -> Identifiers -> Profile -> Async (InternalError errors) Unit
addProfile client playerId identifiers profile @ { fieldValues } = do
    profileId <- addProfile' client playerId identifiers profile
    addFieldValues client profileId fieldValues
