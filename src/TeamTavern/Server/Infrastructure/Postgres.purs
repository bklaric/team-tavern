module TeamTavern.Server.Infrastructure.Postgres where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Error.Class (message, name)
import Node.Errors.Class (code)
import Postgres.Async.Query (query)
import Postgres.Error (Error, constraint, detail, schema, severity, table)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Result (rows)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class ReadForeign)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Log (logLines)

prepareString :: String -> String
prepareString string
    =  "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") string)
    <> "'"

prepareJsonString :: String -> String
prepareJsonString stringValue =
       "\""
    <> (String.replace (String.Pattern "\"") (String.Replacement "") stringValue)
    <> "\""

adjustedTime :: String -> String -> String -> String
adjustedTime timezoneColumn timeColumn timezone = """
    ((current_date || ' ' || """ <> timeColumn <> """ || ' ' || """ <> timezoneColumn <> """)
    ::timestamptz at time zone """ <> prepareString timezone <> """)::time"""

playerAdjustedTime :: String -> String -> String
playerAdjustedTime = adjustedTime "player.timezone"

playerAdjustedWeekdayFrom :: String -> String
playerAdjustedWeekdayFrom = playerAdjustedTime "player.weekday_from"

playerAdjustedWeekdayTo :: String -> String
playerAdjustedWeekdayTo = playerAdjustedTime "player.weekday_to"

playerAdjustedWeekendFrom :: String -> String
playerAdjustedWeekendFrom = playerAdjustedTime "player.weekend_from"

playerAdjustedWeekendTo :: String -> String
playerAdjustedWeekendTo = playerAdjustedTime "player.weekend_to"

teamAdjustedTime :: String -> String -> String
teamAdjustedTime = adjustedTime "team.timezone"

teamAdjustedWeekdayFrom :: String -> String
teamAdjustedWeekdayFrom = teamAdjustedTime "team.weekday_from"

teamAdjustedWeekdayTo :: String -> String
teamAdjustedWeekdayTo = teamAdjustedTime "team.weekday_to"

teamAdjustedWeekendFrom :: String -> String
teamAdjustedWeekendFrom = teamAdjustedTime "team.weekend_from"

teamAdjustedWeekendTo :: String -> String
teamAdjustedWeekendTo = teamAdjustedTime "team.weekend_to"

type InternalError errors = Variant (internal :: Array String | errors)

internalHandler :: forall fields. Lacks "internal" fields =>
    Builder (Record fields) { internal :: Array String -> Effect Unit | fields }
internalHandler = Builder.insert (SProxy :: SProxy "internal") logLines

reportDatabaseError :: forall right errors.
    Async Error right -> Async (InternalError errors) right
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

queryMany :: forall querier errors rows. Querier querier => ReadForeign rows =>
    querier -> Query -> Array QueryParameter -> Async (InternalError errors) (Array rows)
queryMany pool queryString parameters = do
    result <- pool # query queryString parameters # reportDatabaseError
    rows result # traverse read # labelMap (SProxy :: SProxy "internal") \errors ->
        [ "Error reading result from database: " <> show errors ]

queryFirst :: forall row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (InternalError errors) row
queryFirst pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    rows # head # note (inj (SProxy :: SProxy "internal")
        [ "Expected at least one row from database, got none." ])
