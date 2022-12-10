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
import Jarilo (BadRequestRow_, InternalRow_, NotAuthorizedRow_, NotFoundRow_, badRequest__, internal__, notAuthorized__, notFound__)
import Jarilo.Router.Response (AppResponse)
import Node.Errors.Class (code)
import Postgres.Async.Pool (withTransaction)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (constraint, detail, schema, severity, table)
import Postgres.Error as Postgres
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query, QueryParameter)
import Postgres.Result (Result, rows)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
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

type LoadSingleError errors = TerrorVar (InternalRow_ + NotFoundRow_ + errors)

type ChangeSingleError errors = TerrorVar (InternalRow_ + NotAuthorizedRow_ + errors)

databaseErrorLines :: Postgres.Error -> Array String
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

reportDatabaseError :: ∀ right errors.
    Async Postgres.Error right -> Async (InternalTerror_ errors) right
reportDatabaseError = lmap (databaseErrorLines >>> Terror internal__)

queryInternal :: ∀ querier errors. Querier querier =>
    querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) Result
queryInternal querier queryString parameters =
    query queryString parameters querier # reportDatabaseError

queryMany :: ∀ querier errors rows. Querier querier => ReadForeign rows =>
    querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) (Array rows)
queryMany pool queryString parameters = do
    result <- queryInternal pool queryString parameters
    rows result # traverse read # lmap \errors -> Terror internal__
        [ "Error reading result from database: " <> show errors ]

queryMany_ :: ∀ querier errors rows. Querier querier => ReadForeign rows =>
    querier -> Query -> Async (InternalTerror_ errors) (Array rows)
queryMany_ pool queryString = queryMany pool queryString []

queryFirst
    :: ∀ row querier errors
    .  Querier querier
    => ReadForeign row
    => Variant (internal :: AppResponse Unit | errors)
    -> querier
    -> Query
    -> Array QueryParameter
    -> Async (InternalTerror_ errors) row
queryFirst response pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    rows # head # note (Terror response [ "Expected at least one row from database, got none." ])

queryFirstInternal :: ∀ row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) row
queryFirstInternal = queryFirst internal__

queryFirstInternal_ :: ∀ row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Async (InternalTerror_ errors) row
queryFirstInternal_ pool queryString = queryFirst internal__ pool queryString []

queryFirstBadRequest :: ∀ row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (TerrorVar (InternalRow_ + BadRequestRow_ + errors)) row
queryFirstBadRequest = queryFirst badRequest__

queryFirstNotFound :: ∀ row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (LoadSingleError errors) row
queryFirstNotFound = queryFirst notFound__

queryFirstMaybe :: ∀ errors querier row. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) (Maybe row)
queryFirstMaybe pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    pure $ head rows

queryFirstNotAuthorized :: ∀ row errors querier. Querier querier => ReadForeign row =>
    querier -> Query -> Array QueryParameter -> Async (ChangeSingleError errors) row
queryFirstNotAuthorized = queryFirst notAuthorized__

queryNone :: ∀ querier errors. Querier querier =>
    querier -> Query -> Array QueryParameter -> Async (InternalTerror_ errors) Unit
queryNone querier queryString parameters =
    querier # execute queryString parameters # reportDatabaseError

transaction :: ∀ result errors.
    (Client -> Async (InternalTerror_ errors) result) -> Pool -> Async (InternalTerror_ errors) result
transaction = withTransaction (databaseErrorLines >>> Terror internal__)
