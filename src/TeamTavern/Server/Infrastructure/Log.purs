module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Data.Variant (class VariantMatchCases, SProxy(..), Variant, match)
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Error.Class (message, name)
import Node.Errors.Class (class NodeError, code)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Error (InternalError, LoadSingleError)

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

logError
    :: forall cases fields' fields fieldList
    .  RowToList fields fieldList
    => VariantMatchCases fieldList fields' (Effect Unit)
    => Union fields' () cases
    => String
    -> Builder (Record ()) (Record fields)
    -> Variant cases
    -> Effect Unit
logError heading handler error = do
    logStamped heading
    match (Builder.build handler {}) error

internalHandler :: forall fields. Lacks "internal" fields =>
    Builder (Record fields) { internal :: Array String -> Effect Unit | fields }
internalHandler = Builder.insert (SProxy :: SProxy "internal") logLines

notFoundHandler :: forall fields. Lacks "notFound" fields =>
    Builder (Record fields) { notFound :: Array String -> Effect Unit | fields }
notFoundHandler = Builder.insert (SProxy :: SProxy "notFound") logLines

notAuthorizedHandler :: forall fields. Lacks "notAuthorized" fields =>
    Builder (Record fields) { notAuthorized :: Array String -> Effect Unit | fields }
notAuthorizedHandler = Builder.insert (SProxy :: SProxy "notAuthorized") logLines

clientHandler :: forall handlers. Lacks "client" handlers =>
    Builder (Record handlers) { client :: Array String -> Effect Unit | handlers }
clientHandler = Builder.insert (SProxy :: SProxy "client") logLines

logInternalError :: String -> InternalError () -> Effect Unit
logInternalError heading error = logError heading internalHandler error

logLoadSingleError :: String -> LoadSingleError () -> Effect Unit
logLoadSingleError heading error =
    logError heading (internalHandler >>> notFoundHandler) error

print :: forall error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
