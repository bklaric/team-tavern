module TeamTavern.Server.Team.ViewByOwner where

import Prelude

import Async (alwaysRight, examineLeftWithEffect, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label, labelMap)
import Data.Functor (mapFlipped)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (inj, match)
import Effect (foreachE)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import Perun.Response (internalServerError__, ok_)
import Postgres.Async.Query (query)
import Postgres.Error (constraint, detail, schema, severity, table)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rows)
import Simple.JSON (writeJSON)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt)

reportDatabaseError = labelMap (SProxy :: SProxy "internal") \error ->
    [ "Name: " <> name error
    , "Message: " <> message error
    , "Code: " <> code error
    , "Severity: " <> severity error
    , "Details: " <> detail error
    , "Schema: " <> maybe "-" identity (schema error)
    , "Table: " <> maybe "-" identity (table error)
    , "Constraint: " <> maybe "-" identity (constraint error)
    ]

reportResultError = labelMap (SProxy :: SProxy "internal") \errors ->
    [ "Error reading result from database: " <> show errors ]

queryMany pool queryString parameters = do
    result <- pool # query queryString parameters # reportDatabaseError
    rows result # traverse read # reportResultError

type Team = { name :: String, handle :: String }

queryString = Query """
    select team.name, team.handle
    from team
        join player on player.id = team.owner_id
    where lower(player.nickname) = lower($1);
    """

loadTeams pool nickname = queryMany pool queryString (nickname : [])

logLines = (flip foreachE) logt

logError error = do
    logStamped "Error viewing teams by owner"
    error # match { internal: logLines }

sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

viewByOwner pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    -- Load teams from database by owner and just send them lol.
    teams :: Array Team <- loadTeams pool routeParams.nickname
    pure teams
