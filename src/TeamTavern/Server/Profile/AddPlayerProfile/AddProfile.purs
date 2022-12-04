module TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Infrastructure.Error (InternalTerror_)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import Yoga.JSON (writeImpl)

queryString :: Query
queryString = Query """
    insert into player_profile
        (player_id, game_id, platform, new_or_returning, about, ambitions)
    select $1, game.id, $3, $4, $5, $6
    from game
    where game.handle = $2
    returning player_profile.id as "profileId";
    """

queryParameters :: Int -> AddPlayerProfile.RouteParams -> Profile -> Array QueryParameter
queryParameters playerId { handle }
    { platform, newOrReturning, about, ambitions } =
    playerId : handle : writeImpl platform : newOrReturning : about :| ambitions

addProfile' :: forall errors.
    Client -> Int -> AddPlayerProfile.RouteParams -> Profile -> Async (InternalTerror errors) ProfileId
addProfile' client playerId identifiers profile = do
    { profileId } :: { profileId :: Int } <- queryFirstInternal client queryString
        (queryParameters playerId identifiers profile)
    pure profileId

addProfile :: forall errors.
    Client -> Int -> AddPlayerProfile.RouteParams -> Profile -> Async (InternalTerror errors) Int
addProfile client playerId identifiers profile @ { fieldValues } = do
    profileId <- addProfile' client playerId identifiers profile
    addFieldValues client profileId fieldValues
    pure profileId
