module TeamTavern.Profile.ViewByGame.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, toQueryParameter)
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.Domain.Summary (Summary)

type LoadProfilesDto =
    { nickname :: String
    , summary :: Array String
    }

type LoadProfilesResult =
    { nickname :: Nickname
    , summary :: Summary
    }

type LoadProfilesError errors = Variant
    ( databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

queryString :: Query
queryString = Query """
    select game.handle, player.nickname, profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where game.handle = $1
    order by profile.created desc
    """

queryParameters :: Handle -> Array QueryParameter
queryParameters handle = [toQueryParameter $ unwrap handle]

loadProfiles
    :: forall errors
    .  Pool
    -> Handle
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool handle = do
    result <- pool
        # query queryString (queryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ nickname, summary } ->
        { nickname: wrap nickname
        , summary: summary <#> wrap # wrap
        }
