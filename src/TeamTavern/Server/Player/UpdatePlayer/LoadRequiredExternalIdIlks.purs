module TeamTavern.Server.Player.UpdatePlayer.LoadRequiredExternalIdIlks (loadRequiredExternalIdIlks) where

import Prelude

import Async (Async)
import Postgres.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)

queryString :: Query
queryString = Query """
    select distinct game.external_id_ilk as "externalIdIlk"
    from game join player_profile on player_profile.game_id = game.id
    where player_profile.player_id = $1
    """

loadRequiredExternalIdIlks :: forall errors querier. Querier querier =>
    querier -> Int -> Async (InternalError errors) (Array Int)
loadRequiredExternalIdIlks querier playerId =
    queryMany querier queryString (playerId : [])
    <#> \(rows :: Array { externalIdIlk :: Int }) -> rows <#> _.externalIdIlk
