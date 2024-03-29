module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Data.Array as Array
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Error.Class (message, name)
import Node.Errors.Class (class NodeError, code)
import TeamTavern.Server.Infrastructure.Error (Terror(..))

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

logError :: ∀ errors. String -> Terror errors -> Effect Unit
logError heading (Terror _ lines) =
    logStamped $ String.joinWith " | " $ Array.cons heading lines

print :: ∀ error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
