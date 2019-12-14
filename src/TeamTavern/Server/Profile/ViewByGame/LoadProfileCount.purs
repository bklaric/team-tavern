module TeamTavern.Server.Profile.ViewByGame.LoadProfileCount where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (createProfilesFilterString, sanitizeStringValue)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)

type LoadProfileCountResult = { count :: Int }

type LoadProfileCountError errors = Variant
    ( unreadableCount ::
        { count :: Foreign
        , errors :: MultipleErrors
        }
    , noRowsSomehow :: Result
    , databaseError :: Error
    | errors )

queryString :: String -> Int -> QueryPairs Key Value -> Query
queryString handle ilk (QueryPairs filters) = let
        -- Prepare game handle.
    preparedHandle = sanitizeStringValue handle

    -- Create profiles filter string.
    filterString = createProfilesFilterString preparedHandle ilk filters

    -- Insert it into the rest of the query.
    in
    Query $ """
    select count(*)::int as "count"
    from profile
        join game on game.id = profile.game_id
    """ <> filterString

loadProfileCount
    :: forall errors
    .  Client
    -> String
    -> Int
    -> QueryPairs Key Value
    -> Async (LoadProfileCountError errors) Int
loadProfileCount client handle ilk filters = do
    result <- client
        # query (queryString handle ilk filters) []
        # label (SProxy :: SProxy "databaseError")
    count <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "noRowsSomehow") result)
    count' :: LoadProfileCountResult <- read count
        # labelMap (SProxy :: SProxy "unreadableCount") { count, errors: _ }
    pure count'.count