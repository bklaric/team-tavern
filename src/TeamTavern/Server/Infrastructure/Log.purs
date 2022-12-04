module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Data.Variant (class VariantMatchCases, Variant, match)
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Error.Class (message, name)
import Heterogeneous.Mapping (class HMap, hmap)
import Node.Errors.Class (class NodeError, code)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Error (CommonError, InternalError, InternalError_, TavernError(..))
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError)
import Type.Proxy (Proxy(..))

datetimeFormat :: List FormatterCommand
datetimeFormat =
    YearFull : Placeholder "-" :
    MonthTwoDigits : Placeholder "-" :
    DayOfMonthTwoDigits : Placeholder " " :
    Hours24 : Placeholder ":" :
    MinutesTwoDigits : Placeholder ":" :
    SecondsTwoDigits : Nil

logStamped :: String -> Effect Unit
logStamped string =
    nowDateTime
    <#> format datetimeFormat
    >>= \dateTime -> log $ dateTime <> " - " <> string

logt :: String -> Effect Unit
logt string = log $ "    " <> string

logLines :: Array String -> Effect Unit
logLines lines = foreachE lines logt

-- logError
--     :: forall cases fields' fields fieldList
--     .  RowToList fields fieldList
--     => VariantMatchCases fieldList fields' (Effect Unit)
--     => Union fields' () cases
--     => String
--     -> Builder (Record ()) (Record fields)
--     -> Variant cases
--     -> Effect Unit
logError :: forall errors. String -> TavernError errors -> Effect Unit
logError heading (TavernError _ lines) = do
    logStamped heading
    -- match (Builder.build handler {}) error
    logLines lines

-- internalHandler :: forall fields. Lacks "internal" fields =>
--     Builder (Record fields) { internal :: Array String -> Effect Unit | fields }
-- internalHandler = Builder.insert (Proxy :: _ "internal") logLines

-- notFoundHandler :: forall fields. Lacks "notFound" fields =>
--     Builder (Record fields) { notFound :: Array String -> Effect Unit | fields }
-- notFoundHandler = Builder.insert (Proxy :: _ "notFound") logLines

-- notAuthenticatedHandler :: forall fields. Lacks "notAuthenticated" fields =>
--     Builder (Record fields) { notAuthenticated :: Array String -> Effect Unit | fields }
-- notAuthenticatedHandler = Builder.insert (Proxy :: _ "notAuthenticated") logLines

-- notAuthorizedHandler :: forall fields. Lacks "notAuthorized" fields =>
--     Builder (Record fields) { notAuthorized :: Array String -> Effect Unit | fields }
-- notAuthorizedHandler = Builder.insert (Proxy :: _ "notAuthorized") logLines

-- clientHandler :: forall handlers. Lacks "client" handlers =>
--     Builder (Record handlers) { client :: Array String -> Effect Unit | handlers }
-- clientHandler = Builder.insert (Proxy :: _ "client") logLines

-- commonHandler
--     :: forall handlers
--     .  Lacks "internal" handlers
--     => Lacks "notFound" handlers
--     => Lacks "notAuthenticated" handlers
--     => Lacks "notAuthorized" handlers
--     => Lacks "client" handlers
--     => Builder (Record handlers)
--         { client :: Array String -> Effect Unit
--         , internal :: Array String -> Effect Unit
--         , notAuthenticated :: Array String -> Effect Unit
--         , notAuthorized :: Array String -> Effect Unit
--         , notFound :: Array String -> Effect Unit
--         | handlers
--         }
-- commonHandler =
--     internalHandler
--     >>> notFoundHandler
--     >>> notAuthenticatedHandler
--     >>> notAuthorizedHandler
--     >>> clientHandler

-- logError :: String -> InternalError_ () -> Effect Unit
-- logError heading error = logError heading error

-- logError :: String -> LoadSingleError () -> Effect Unit
-- logError heading error = logError heading error

-- logCommonError :: String -> CommonError -> Effect Unit
-- logCommonError heading error = logError heading error

print :: forall error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error

elaborate
    :: forall line right left bifunctor
    .  Bifunctor bifunctor => HMap (Array line -> Array line) left left
    => line -> bifunctor left right -> bifunctor left right
elaborate line = lmap (hmap (_ <> [ line ]))
