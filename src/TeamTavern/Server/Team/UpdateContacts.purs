module TeamTavern.Server.Team.UpdateContacts (updateContacts) where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Team.UpdateTeamContacts as UpdateTeamContacts
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInOwner (ensureSignedInOwner)
import TeamTavern.Server.Infrastructure.Postgres (queryMany, transaction)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Team.Infrastructure.ValidateContacts (validateContacts)
import TeamTavern.Server.Team.Infrastructure.WriteContacts (writeContacts)

-- Load plaftorms

queryString :: Query
queryString = Query $ """
    select distinct unnest(team_profile.platforms) as platform
    from team_profile
    where team_profile.team_id = $1;
    """

loadRequiredPlatforms :: forall querier errors. Querier querier =>
    querier -> Int -> Async (InternalTerror_ errors) (Array Platform)
loadRequiredPlatforms querier id =
    (queryMany querier queryString (id : []) :: Async _ (Array { platform :: Platform }))
    <#> map _.platform

-- Main
updateContacts :: forall left.
    Pool -> Cookies -> { handle :: String } -> UpdateTeamContacts.RequestContent -> Async left _
updateContacts pool cookies { handle } contacts' =
    sendResponse "Error updating team contacts" do
    pool # transaction \client -> do
        -- Read requestor info from cookies.
        { teamId } <- ensureSignedInOwner client cookies handle

        -- Read required platforms.
        requiredPlatforms <- loadRequiredPlatforms client teamId

        -- Validate contacts.
        contacts <- validateContacts requiredPlatforms contacts'

        -- Update contacts.
        writeContacts client teamId contacts
    pure noContent_
