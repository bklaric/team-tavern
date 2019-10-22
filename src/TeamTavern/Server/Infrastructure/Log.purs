module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Error.Class (message, name)
import Node.Errors.Class (class NodeError, code)

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

print :: forall error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
