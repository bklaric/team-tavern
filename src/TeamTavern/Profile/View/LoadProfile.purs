module TeamTavern.Profile.View.LoadProfile
    (ViewModel, LoadProfileError, loadProfile) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Profile.Domain.Summary as Summary
import TeamTavern.Profile.Domain.Types (Identifiers)
import TeamTavern.Profile.Domain.Types as Profile

type ViewModel = { summary :: String }

type LoadProfileError errors = Variant
    ( databaseError :: Error
    , unreadableViewModel ::
        { foreignViewModel :: Foreign
        , errors :: MultipleErrors
        }
    , notFound :: Identifiers
    , invalidView :: ViewModel
    | errors )

loadProfileQuery :: Query
loadProfileQuery = Query """
    select profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where player.nickname = $1
    and game.handle = $2
    """

loadProfileParameters :: Identifiers -> Array QueryParameter
loadProfileParameters { nickname, handle } =
    [unwrap nickname, unwrap handle] <#> QueryParameter

loadProfile :: forall errors.
    Pool -> Identifiers -> Async (LoadProfileError errors) Profile.View
loadProfile pool identifiers = do
    result <- pool
        # query loadProfileQuery (loadProfileParameters identifiers)
        # label (SProxy :: SProxy "databaseError")
    foreignViewModel <- rows result
        # head
        # Async.note (inj (SProxy :: SProxy "notFound") identifiers)
    viewModel @ { summary } :: ViewModel <- read foreignViewModel
        # labelMap (SProxy :: SProxy "unreadableViewModel")
            { foreignViewModel, errors: _ }
    { summary: _ }
        <$> Summary.create'' summary
        # Async.note (inj (SProxy :: SProxy "invalidView") viewModel)
