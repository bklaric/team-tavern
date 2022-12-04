module TeamTavern.Server.Infrastructure.Postgres where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Error.Class (message, name)
import Jarilo (badRequest__, internal__, notAuthorized__, notFound__)
import Jarilo.Router.Response (AppResponse)
import Node.Errors.Class (code)
import Postgres.Async.Pool (withTransaction)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (Error, constraint, detail, schema, severity, table)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Error (InternalError_, InternalRow_, NotAuthorizedRow_, NotFoundRow_, TavernError(..), BadRequestRow_)
import Type.Row (type (+))
import Yoga.JSON (class ReadForeign)
import Yoga.JSON.Async (read)

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

type LoadSingleError errors = TavernError (InternalRow_ + NotFoundRow_ + errors)

type ChangeSingleError errors = TavernError (InternalRow_ + NotAuthorizedRow_ + errors)

databaseErrorLines :: Error -> Array String
databaseErrorLines error =
    [ "Name: " <> name error
    , "Message: " <> message error
    , "Code: " <> code error
    , "Severity: " <> severity error
    , "Details: " <> detail error
    , "Schema: " <> maybe "-" identity (schema error)
    , "Table: " <> maybe "-" identity (table error)
    , "Constraint: " <> maybe "-" identity (constraint error)
    ]

reportDatabaseError :: forall right errors.
    Async Error right -> Async (InternalError_ errors) right
reportDatabaseError = lmap (databaseErrorLines >>> TavernError internal__)

queryInternal :: forall querier errors. Querier querier =>
    querier -> Query -> Array QueryParameter -> Async (InternalError_ errors) Result
queryInternal querier queryString parameters =
    query queryString parameters querier # reportDatabaseError

queryMany :: forall querier errors rows. Querier querier => ReadForeign rows =>
    querier -> Query -> Array QueryParameter -> Async (InternalError_ errors) (Array rows)
queryMany pool queryString parameters = do
    result <- queryInternal pool queryString parameters
    rows result # traverse read # lmap \errors -> TavernError internal__
        [ "Error reading result from database: " <> show errors ]

queryMany_ :: forall querier errors rows. Querier querier => ReadForeign rows =>
    querier -> Query -> Async (InternalError_ errors) (Array rows)
queryMany_ pool queryString = queryMany pool queryString []

queryFirst
    :: forall row querier errors
    .  Querier querier
    => ReadForeign row
    => Variant (internal :: AppResponse Unit | errors)
    -> querier
    -> Query
    -> Array QueryParameter
    -> Async (InternalError_ errors) row
queryFirst response pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    rows # head # note (TavernError response [ "Expected at least one row from database, got none." ])

queryFirstInternal :: forall row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (InternalError_ errors) row
queryFirstInternal = queryFirst internal__

queryFirstBadRequest :: forall row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (TavernError (InternalRow_ + BadRequestRow_ + errors)) row
queryFirstBadRequest = queryFirst badRequest__

queryFirstNotFound :: forall row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (LoadSingleError errors) row
queryFirstNotFound = queryFirst notFound__

queryFirstMaybe :: forall errors querier row. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (InternalError_ errors) (Maybe row)
queryFirstMaybe pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    pure $ head rows

queryFirstNotAuthorized :: forall row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (ChangeSingleError errors) row
queryFirstNotAuthorized = queryFirst notAuthorized__

queryNone :: forall querier errors. Querier querier =>
    querier -> Query -> Array QueryParameter -> Async (InternalError_ errors) Unit
queryNone querier queryString parameters =
    querier # execute queryString parameters # reportDatabaseError

transaction :: forall result errors.
    (Client -> Async (InternalError_ errors) result) -> Pool -> Async (InternalError_ errors) result
transaction = withTransaction (databaseErrorLines >>> TavernError internal__)
