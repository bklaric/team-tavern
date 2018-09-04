module TeamTavern.Profile.ViewByGame.LoadProfiles where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (Variant)
import Foreign (Foreign, ForeignError)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Profile.Domain.Summary as Summary
import TeamTavern.Profile.Domain.Types (ByGameView)

type LoadProfilesError errors = Variant
    ( invalidViews :: NonEmptyList
        { nickname :: String
        , summary :: String
        }
    , databaseError :: Error
    , unreadableResult ::
        { rows :: Array Foreign
        , errors :: NonEmptyList ForeignError
        }
    | errors
    )

loadProfilesQuery :: Query
loadProfilesQuery = Query """
    select player.nickname, profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where game.handle = $1
    """

loadProfilesParameters :: Handle -> Array QueryParameter
loadProfilesParameters handle = [unwrap handle] <#> QueryParameter

type ByGameViewModel = { nickname :: String, summary :: String }

validateProfiles :: forall errors.
    Array ByGameViewModel -> Async (LoadProfilesError errors) (Array ByGameView)
validateProfiles profiles = let
    validateProfile ::
        ByGameViewModel -> Validated (NonEmptyList ByGameViewModel) ByGameView
    validateProfile profile @ { nickname, summary } =
        { nickname: _, summary: _ }
        <$> (Nickname.create nickname # Validated.hush)
        <*> (Summary.create summary # Validated.hush)
        # Validated.note' profile
    in
    profiles
    # traverse validateProfile
    # Async.fromValidated
    # label (SProxy :: SProxy "invalidViews")

loadProfiles :: forall errors.
    Pool -> Handle -> Async (LoadProfilesError errors) (Array ByGameView)
loadProfiles pool handle = do
    result <- pool
        # query loadProfilesQuery (loadProfilesParameters handle)
        # label (SProxy :: SProxy "databaseError")
    profiles <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult")
            { rows: rows result, errors: _ }
    validateProfiles profiles
