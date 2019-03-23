module TeamTavern.Profile.View.LoadProfile
    (LoadProfileResult, LoadProfileError, loadProfile) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, toQueryParameter)
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Profile.Domain.Summary (Summary)
import TeamTavern.Profile.Routes (Identifiers)

type LoadProfileDto = { summary :: Array String }

type LoadProfileResult = { summary :: Summary }

type LoadProfileError errors = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { foreignDto :: Foreign
        , errors :: MultipleErrors
        }
    , notFound :: Identifiers
    | errors )

loadProfileQuery :: Query
loadProfileQuery = Query """
    select profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where game.handle = $1
    and player.nickname = $2
    """

loadProfileParameters :: Identifiers -> Array QueryParameter
loadProfileParameters { handle, nickname } =
    [unwrap handle, unwrap nickname] <#> toQueryParameter

loadProfile
    :: forall errors
    .  Pool
    -> Identifiers
    -> Async (LoadProfileError errors) LoadProfileResult
loadProfile pool identifiers = do
    result <- pool
        # query loadProfileQuery (loadProfileParameters identifiers)
        # label (SProxy :: SProxy "databaseError")
    foreignDto <- rows result
        # head
        # Async.note (inj (SProxy :: SProxy "notFound") identifiers)
    viewModel @ { summary } :: LoadProfileDto <- read foreignDto
        # labelMap (SProxy :: SProxy "unreadableDto")
            { foreignDto, errors: _ }
    pure { summary: summary <#> wrap # wrap }
