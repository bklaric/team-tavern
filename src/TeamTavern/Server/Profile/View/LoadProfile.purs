module TeamTavern.Server.Profile.View.LoadProfile
    (LoadProfileResult, LoadProfileError, loadProfile) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:|))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Routes (Identifiers)

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

queryString :: Query
queryString = Query """
    select profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where game.handle = $1
    and player.nickname = $2
    """

queryParameters :: Identifiers -> Array QueryParameter
queryParameters { handle, nickname } = handle :| nickname

loadProfile
    :: forall errors
    .  Pool
    -> Identifiers
    -> Async (LoadProfileError errors) LoadProfileResult
loadProfile pool identifiers = do
    result <- pool
        # query queryString (queryParameters identifiers)
        # label (SProxy :: SProxy "databaseError")
    foreignDto <- rows result
        # head
        # Async.note (inj (SProxy :: SProxy "notFound") identifiers)
    viewModel @ { summary } :: LoadProfileDto <- read foreignDto
        # labelMap (SProxy :: SProxy "unreadableDto")
            { foreignDto, errors: _ }
    pure { summary: summary <#> wrap # wrap }
