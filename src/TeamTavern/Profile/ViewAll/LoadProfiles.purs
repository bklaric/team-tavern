module TeamTavern.Profile.ViewAll.LoadProfiles
    (LoadProfilesResult, LoadProfilesError, loadProfiles) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor.Label (label, labelMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.Domain.Summary (Summary)
import TeamTavern.Profile.Routes (IdentifiersMany)

type QueryWithParameters =
    { query :: Query
    , parameters :: Array QueryParameter
    }

type LoadProfilesDto =
    { handle :: String
    , nickname :: String
    , summary :: String
    }

type LoadProfilesResult =
    { handle :: Handle
    , nickname :: Nickname
    , summary :: Summary
    }

type LoadProfilesError errors = Variant
    ( invalidFilters :: IdentifiersMany
    , databaseError :: Error
    , unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

loadProfilesQuery :: String -> Query
loadProfilesQuery whereClause = Query $ """
    select game.handle, player.nickname, profile.summary
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where """ <> whereClause <> """
    order by profile.created desc
    """

queryWithParameters :: forall errors.
    IdentifiersMany -> Either (LoadProfilesError errors) QueryWithParameters
queryWithParameters identifiers =
    case identifiers of
    { handle: Just handle, nickname: Nothing } -> Right $
        { query: loadProfilesQuery "game.handle = $1"
        , parameters: [QueryParameter $ unwrap handle]
        }
    { handle: Nothing, nickname: Just nickname } -> Right $
        { query: loadProfilesQuery "player.nickname = $1"
        , parameters: [QueryParameter $ unwrap nickname]
        }
    _ -> Left $ inj (SProxy :: SProxy "invalidFilters") identifiers

loadProfiles
    :: forall errors
    .  Pool
    -> IdentifiersMany
    -> Async (LoadProfilesError errors) (Array LoadProfilesResult)
loadProfiles pool identifiers = do
    { query: queryString, parameters } <- queryWithParameters identifiers
        # fromEither
    result <- pool
        # query queryString parameters
        # label (SProxy :: SProxy "databaseError")
    profiles :: Array LoadProfilesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ profiles <#> \{ handle, nickname, summary } ->
        { handle: wrap handle
        , nickname: wrap nickname
        , summary: wrap summary
        }
