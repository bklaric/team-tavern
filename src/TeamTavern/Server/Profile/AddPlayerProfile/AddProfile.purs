module TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile) where

import Prelude

import Async (Async)
import Postgres.Client (Client)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import TeamTavern.Server.Domain.Text (Text)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal)
import TeamTavern.Server.Profile.AddPlayerProfile.AddFieldValues (ProfileId, addFieldValues)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateExternalId (ExternalId)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateExternalId as ExternalId
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (Profile)
import TeamTavern.Server.Profile.Routes (Identifiers)

queryString :: Query
queryString = Query """
    insert into player_profile (player_id, game_id, external_id, ambitions, new_or_returning)
    select $1, game.id, $3, $4, $5
    from game
    where game.handle = $2
    returning player_profile.id as "profileId";
    """

queryParameters :: Int -> Identifiers -> ExternalId -> Text -> Boolean -> Array QueryParameter
queryParameters playerId { handle, nickname } externalId ambitions newOrReturning =
    playerId : handle : ExternalId.toString externalId : ambitions :| newOrReturning

addProfile'
    :: forall errors
    .  Client
    -> Int
    -> Identifiers
    -> ExternalId
    -> Text
    -> Boolean
    -> Async (InternalError errors) ProfileId
addProfile' client playerId identifiers externalId ambitions newOrReturning = do
    { profileId } :: { profileId :: Int } <- queryFirstInternal client queryString
        (queryParameters playerId identifiers externalId ambitions newOrReturning)
    pure profileId

addProfile :: forall errors.
    Client -> Int -> Identifiers -> Profile -> Async (InternalError errors) Unit
addProfile client playerId identifiers { externalId, fieldValues, newOrReturning, ambitions } = do
    profileId <- addProfile' client playerId identifiers externalId ambitions newOrReturning
    addFieldValues client profileId fieldValues
