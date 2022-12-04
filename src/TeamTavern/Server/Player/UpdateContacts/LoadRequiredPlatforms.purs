module TeamTavern.Server.Player.UpdateContacts.LoadRequiredPlatforms (loadRequiredPlatforms) where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

queryString :: Query
queryString = Query $ """
    select distinct player_profile.platform
    from player_profile
    where player_profile.player_id = $1;
    """

loadRequiredPlatforms :: forall querier errors. Querier querier =>
    querier -> Int -> Async (InternalError_ errors) (Array Platform)
loadRequiredPlatforms querier id =
    (queryMany querier queryString (id : []) :: Async _ (Array { platform :: Platform }))
    <#> map _.platform
