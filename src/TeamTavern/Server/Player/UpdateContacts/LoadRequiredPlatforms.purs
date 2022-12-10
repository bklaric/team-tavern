module TeamTavern.Server.Player.UpdateContacts.LoadRequiredPlatforms (loadRequiredPlatforms) where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

queryString :: Query
queryString = Query $ """
    select distinct player_profile.platform
    from player_profile
    where player_profile.player_id = $1;
    """

loadRequiredPlatforms :: ∀ querier errors. Querier querier =>
    querier -> Int -> Async (InternalTerror_ errors) (Array Platform)
loadRequiredPlatforms querier id =
    (queryMany querier queryString (id : []) :: Async _ (Array { platform :: Platform }))
    <#> map _.platform
